# The chart spec
#
# With options(geneviewer.record_spec = TRUE), GC_chart() starts x$spec and
# every GC_ function adds the arguments it was called with, under its key and
# with the names of the spec (spec v1, schema/NAMING.md in geneviewer-js).
# Arguments are recorded as given: no defaults are filled in and nothing is
# recycled or expanded. Data processing (GC_links(group = ), GC_transcript(),
# GC_normalize()) records its results as data. The option is off by default;
# it does not change the chart.

#' Whether the spec is recorded
#' @noRd
recording_spec <- function() {
  isTRUE(getOption("geneviewer.record_spec", FALSE))
}

#' The arguments given in a call, for the spec
#'
#' Call it first in a GC_ function, before any argument is changed.
#'
#' @param call The call, from `match.call()`.
#' @param env The environment of the function, from `environment()`.
#' @param fun The function, for the defaults of its arguments.
#' @return A named list of the arguments given in the call, without the chart
#'   and without empty lists; NULL when the spec is not recorded. A NULL is
#'   kept only where the argument's default is not NULL (e.g.
#'   `GC_pattern(background = NULL)`). The attribute "dots" names the
#'   arguments passed through `...`, which keep their names.
#' @noRd
spec_args <- function(call, env, fun = sys.function(-1)) {
  if (!recording_spec()) {
    return(NULL)
  }
  # The chart first, as the function itself would
  if (exists("GC_chart", envir = env, inherits = FALSE)) get("GC_chart", envir = env)
  given <- setdiff(names(call)[-1], c("", "GC_chart"))
  formal <- intersect(given, setdiff(ls(env, all.names = TRUE), "..."))
  args <- mget(formal, envir = env)
  defaults <- formals(fun)
  dot_names <- character()
  if (exists("...", envir = env, inherits = FALSE)) {
    dots <- eval(quote(list(...)), env)
    dots <- dots[nzchar(names(dots))]
    dot_names <- names(dots)
    args <- c(args, dots)
  }
  args <- args[order(match(names(args), given))]
  keep <- vapply(names(args), function(name) {
    value <- args[[name]]
    if (is.null(value)) {
      return(name %in% formal && !is.null(defaults[[name]]))
    }
    !(is.list(value) && length(value) == 0)
  }, logical(1))
  args <- args[keep]
  attr(args, "dots") <- intersect(dot_names, names(args))
  args
}

#' An entry of the spec: a named list, also when empty
#' @noRd
spec_entry <- function(args) {
  if (length(args) == 0) {
    return(structure(list(), names = character(0)))
  }
  args
}

#' Whether a spec value is one entry, not an array of entries
#' @noRd
is_spec_entry <- function(x) {
  is.list(x) && !is.null(names(x))
}

#' Spec names of R arguments
#'
#' @param names Character, the R argument names.
#' @param rename Named character, spec names of single arguments.
#' @param clusters Logical, whether `cluster` becomes `clusters`.
#' @noRd
spec_names <- function(names, rename = character(), clusters = TRUE) {
  renamed <- names %in% names(rename)
  names[renamed] <- rename[names[renamed]]
  if (clusters) names[names == "cluster"] <- "clusters"
  names[!renamed] <- snake_to_camel(names[!renamed])
  names
}

#' Spec names of options passed through ... that the spec renames
#'
#' The renderer reads `scaleBarUnit` and `labelAdjustmentOptions`; spec v1
#' names them `scaleBar.unit` and `labelAdjustment` (decided 2026-11-05).
#' @noRd
spec_dot_renames <- list(
  scaleBar = c(scaleBarUnit = "unit"),
  labels = c(labelAdjustmentOptions = "labelAdjustment"),
  links = c(labelAdjustmentOptions = "labelAdjustment")
)

#' @noRd
spec_dot_names <- function(key, names) {
  renames <- spec_dot_renames[[key]]
  hit <- names %in% names(renames)
  names[hit] <- renames[names[hit]]
  names
}

#' Spec names of the options of each component in a theme
#'
#' A theme's component options use the names of the component in the spec
#' (`theme.legend.textStyle`, decided 2026-11-05); R's theme settings use
#' the argument names, camelCased here first.
#' @noRd
spec_theme_renames <- list(
  legend = c(legendOptions = "markerStyle", legendTextOptions = "textStyle"),
  sequence = c(sequenceStyle = "lineStyle"),
  scaleBar = c(scaleBarLineStyle = "lineStyle", scaleBarTickStyle = "tickStyle",
               scaleBarUnit = "unit"),
  links = c(showLinks = "show", labelAdjustmentOptions = "labelAdjustment"),
  labels = c(labelAdjustmentOptions = "labelAdjustment"),
  scale = c(scaleBreaks = "autoBreaks", scaleBreakThreshold = "autoBreakThreshold",
            scaleBreakPadding = "breakPadding")
)

#' Add the arguments of a call to the spec
#'
#' @param GC_chart The gene chart object.
#' @param key Character, the key of the spec, e.g. "labels".
#' @param args The arguments, from `spec_args()`; NULL adds nothing.
#' @param rename Named character, spec names of single arguments.
#' @param mode How a call adds to earlier calls: "append" (an entry; more
#'   calls make an array of entries), "items" (always an array), "merge"
#'   (merged into the one entry) or "replace".
#' @param clusters Logical, whether the `cluster` argument becomes `clusters`.
#' @return The gene chart object.
#' @noRd
add_spec <- function(GC_chart, key, args, rename = character(),
                     mode = c("append", "items", "merge", "replace"),
                     clusters = TRUE) {
  mode <- match.arg(mode)
  if (is.null(args) || is.null(GC_chart$x$spec)) {
    return(GC_chart)
  }
  # Options passed through ... keep their names, as R sends them
  dot_names <- attr(args, "dots")
  is_dot <- names(args) %in% dot_names
  names(args)[!is_dot] <- spec_names(names(args)[!is_dot], rename, clusters)
  names(args)[is_dot] <- spec_dot_names(key, names(args)[is_dot])
  attr(args, "dots") <- NULL
  for (name in intersect(c("clusters", "selection"), names(args))) {
    args[[name]] <- as.list(args[[name]])
  }
  args <- spec_entry(args)

  old <- GC_chart$x$spec[[key]]
  new <- switch(mode,
    append = if (is.null(old)) args else c(if (is_spec_entry(old)) list(old) else old, list(args)),
    items = c(old, list(args)),
    merge = if (is.null(old)) args else spec_entry(utils::modifyList(old, args)),
    replace = args
  )
  GC_chart$x$spec[key] <- list(new)
  GC_chart
}

#' Start the spec of a chart
#'
#' @param data The data of the chart, as given or as read from GenBank files.
#' @param args The arguments of `GC_chart()`, from `spec_args()`.
#' @return The spec, or NULL when it is not recorded.
#' @noRd
start_spec <- function(data, args) {
  if (is.null(args)) {
    return(NULL)
  }
  field_names <- c("start", "end", "cluster", "group", "strand")
  spec <- list(version = 1L, data = data)
  fields <- args[intersect(field_names, names(args))]
  if (length(fields) > 0) spec$fields <- fields
  spec$width <- args$width
  spec$height <- args$height
  if (length(args$style) > 0) spec$style <- args$style
  # save_button = FALSE is the toolbar without the save button
  if (isFALSE(args$save_button)) {
    spec$toolbar <- list(buttons = as.list(setdiff(toolbar_buttons, "save")))
  }
  Filter(Negate(is.null), spec)
}

#' A logical given for a column argument, e.g. GC_labels(FALSE), is `show`
#' @noRd
spec_flag <- function(args, name) {
  value <- args[[name]]
  if (is.logical(value) && length(value) == 1) {
    args[[name]] <- NULL
    args$show <- value
  }
  args
}

#' Add a GC_zoom() call to the spec
#'
#' Calls are merged; a new mode does not keep the range or minimum of the old
#' one, as in GC_zoom().
#' @noRd
add_spec_zoom <- function(GC_chart, args) {
  if (is.null(args) || is.null(GC_chart$x$spec)) {
    return(GC_chart)
  }
  if (!is.null(args$range)) args$range <- as.list(unlist(args$range))
  old <- GC_chart$x$spec$zoom
  if (!is.null(old) && !is.null(args$mode) && !identical(args$mode, old$mode)) {
    old$range <- NULL
    old$min <- NULL
    GC_chart$x$spec$zoom <- spec_entry(old)
  }
  add_spec(GC_chart, "zoom", args, mode = "merge")
}

#' Add a GC_element_style() call to the spec
#'
#' The spec has one entry per call, in order. `reset = TRUE` removes the
#' earlier entries for the element instead.
#' @noRd
add_spec_element_style <- function(GC_chart, args, component, reset) {
  if (is.null(args) || is.null(GC_chart$x$spec)) {
    return(GC_chart)
  }
  args$reset <- NULL
  if (reset) {
    id <- c("rowID", "end", "cluster", "part", "group")
    target <- c(list(component = component),
                stats::setNames(args[c("row_id", "end", "cluster", "part", "group")], id))
    same <- function(entry) {
      entry_component <- if (is.null(entry$component)) "genes" else entry$component
      identical(entry_component, component) &&
        all(vapply(id, function(name) {
          identical(as.character(entry[[name]]), as.character(target[[name]]))
        }, logical(1)))
    }
    items <- Filter(Negate(same), GC_chart$x$spec$elementStyle)
    GC_chart$x$spec$elementStyle <- if (length(items) > 0) items
    return(GC_chart)
  }
  add_spec(GC_chart, "elementStyle", args, mode = "items",
           rename = c(row_id = "rowID"), clusters = FALSE)
}

#' Add a GC_theme() call to the spec
#'
#' A call with a theme starts again from it; a call without one changes the
#' theme set before, merged as in GC_theme(). The options of components are
#' named as in the spec's components (`legendTextOptions` becomes
#' `textStyle`, `corner_radius` becomes `cornerRadius`).
#' @noRd
add_spec_theme <- function(GC_chart, args) {
  if (is.null(args) || is.null(GC_chart$x$spec)) {
    return(GC_chart)
  }
  base <- args$theme
  args$theme <- NULL
  attr(args, "dots") <- NULL
  args <- spec_theme_names(args)
  if (is.list(base)) {
    # A list of theme settings is the theme object itself
    new <- merge_theme_settings(spec_theme_names(base), args)
  } else if (!is.null(base)) {
    new <- c(list(base = base), args)
  } else {
    old <- GC_chart$x$spec$theme
    if (is.character(old)) old <- list(base = old)
    new <- merge_theme_settings(if (is.null(old)) list() else old, args)
  }
  GC_chart$x$spec["theme"] <- list(spec_entry(new))
  GC_chart
}

#' Option names of the components of a theme as in the browser
#' @noRd
spec_theme_names <- function(settings) {
  for (name in intersect(names(settings), c(names(theme_components), "annotations"))) {
    if (name == "annotations") {
      settings[[name]] <- lapply(settings[[name]], camel_case_names)
    } else {
      options <- camel_case_names(settings[[name]])
      renames <- spec_theme_renames[[name]]
      if (length(options) > 0 && !is.null(names(options)) && !is.null(renames)) {
        hit <- names(options) %in% names(renames)
        names(options)[hit] <- renames[names(options)[hit]]
      }
      settings[[name]] <- options
    }
  }
  settings
}
