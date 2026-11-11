# The chart spec
#
# GC_chart() starts the spec in x$spec, and every GC_ function adds the
# arguments it was called with, under its key and with the names of the spec
# (spec v1, schema/NAMING.md in geneviewer-js). Arguments are recorded as
# given: no defaults are filled in and nothing is recycled or expanded. Data
# processing (GC_links(group = ), GC_transcript(), GC_normalize()) records its
# results as data. The spec is what the widget sends: x is list(spec = ),
# drawn by the geneviewer JavaScript library.

#' The arguments given in a call, for the spec
#'
#' Call it first in a GC_ function, before any argument is changed.
#'
#' @param call The call, from `match.call()`.
#' @param env The environment of the function, from `environment()`.
#' @param fun The function, for the defaults of its arguments.
#' @return A named list of the arguments given in the call, without the chart
#'   and without empty lists. A NULL is kept only where the argument's default
#'   is not NULL (e.g. `GC_pattern(background = NULL)`). The attribute "dots"
#'   names the arguments passed through `...`, which keep their names.
#' @noRd
spec_args <- function(call, env, fun = sys.function(-1)) {
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
#' @param args The arguments, from `spec_args()`.
#' @param rename Named character, spec names of single arguments.
#' @param mode How a call adds to earlier calls: "append" (an entry; more
#'   calls make an array of entries), "items" (always an array), "merge"
#'   (merged into the one entry: named lists are merged, other values replace
#'   the old ones and NULL removes them) or "replace".
#' @param clusters Logical, whether the `cluster` argument becomes `clusters`.
#' @return The gene chart object.
#' @noRd
add_spec <- function(GC_chart, key, args, rename = character(),
                     mode = c("append", "items", "merge", "replace"),
                     clusters = TRUE) {
  mode <- match.arg(mode)
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
    # utils::modifyList() would keep an old unnamed list, e.g. the buttons of
    # GC_toolbar(); a new one replaces it (B64)
    merge = if (is.null(old)) args else spec_entry(merge_settings(old, args, keep_null = FALSE)),
    replace = args
  )
  GC_chart$x$spec[key] <- list(new)
  GC_chart
}

#' Start the spec of a chart
#'
#' @param data The data of the chart, as given or as read from GenBank files.
#' @param args The arguments of `GC_chart()`, from `spec_args()`.
#' @return The spec.
#' @noRd
start_spec <- function(data, args) {
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
    new <- merge_spec_theme(if (is.null(old)) list() else old, args)
  }
  GC_chart$x$spec["theme"] <- list(spec_entry(new))
  GC_chart
}

#' Theme settings merged as merge_theme_settings() does, but a NULL stays, as
#' null in the spec: the library removes the setting from the theme it is
#' merged into (the built-in theme of `base`)
#' @noRd
merge_spec_theme <- function(x, y) {
  merge_settings(x, y, keep_null = TRUE)
}

#' Option names of the components of a theme as in the browser
#' @noRd
spec_theme_names <- function(settings) {
  for (name in intersect(names(settings), c(theme_components, "annotations"))) {
    if (is.null(settings[[name]])) next
    if (name == "annotations") {
      settings[[name]] <- lapply(settings[[name]], camel_case_names)
    } else {
      options <- camel_case_names(settings[[name]])
      renames <- spec_theme_renames[[name]]
      if (length(options) > 0 && !is.null(names(options)) && !is.null(renames)) {
        hit <- names(options) %in% names(renames)
        names(options)[hit] <- renames[names(options)[hit]]
      }
      # An option given under both names, e.g. a built-in theme's textStyle and
      # a legendTextOptions added to it: one option, the later one merged in
      if (anyDuplicated(names(options))) {
        merged <- list()
        for (i in seq_along(options)) {
          merged <- merge_settings(merged, options[i], keep_null = TRUE)
        }
        options <- merged
      }
      settings[[name]] <- options
    }
  }
  settings
}

#' The data of a chart as GC_chart() prepares it
#'
#' Checks the columns, leaves out rows without a start or end, and adds the
#' columns `rowID`, `start`, `end` and `strand`; cluster names become
#' character. The library does the same with the spec's data; R needs it to
#' check arguments against the data and to process it (GC_links(group = ),
#' GC_transcript(), GC_normalize(), GC_align()).
#'
#' @param data A data frame.
#' @param start,end,cluster,group,strand Column names, as for `GC_chart()`.
#' @param warn Logical, whether to warn about rows left out.
#' @return The data frame.
#' @noRd
prepare_data <- function(data, start = "start", end = "end", cluster = NULL,
                         group = NULL, strand = NULL, warn = TRUE) {
  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # Check if column names are in the data frame
  colnames_data <- colnames(data)

  if (!(start %in% colnames_data)) stop("start column not found in data")
  if (!(end %in% colnames_data)) stop("end column not found in data")

  if (!is.null(cluster) && !(cluster %in% colnames_data)) {
    stop("cluster column not found in data")
  }
  if (!is.null(group) && !(group %in% colnames_data)) {
    stop("group column not found in data")
  }
  if (!is.null(strand) && !(strand %in% colnames_data)) {
    stop("strand column not found in data")
  }

  # Filter out rows with NA values in 'start' or 'end' columns
  original_nrow <- nrow(data)
  data <- data[!is.na(data[[start]]) & !is.na(data[[end]]), ]

  # Check if any rows were filtered out and issue a warning
  if (warn && nrow(data) < original_nrow) {
    warning("Rows with NA values in 'start' or 'end' columns have been filtered out.")
  }

  # Add rowID to data
  data$rowID <- seq_len(nrow(data))

  # add start and end
  data_tmp <- data
  data$start <- data_tmp[[start]]
  data$end <- data_tmp[[end]]

  # Convert cluster to character
  if (!is.null(cluster)) {
    data[[cluster]] <- as.character(data[[cluster]])
  }

  # Add strand if specified
  add_strand(data, strand)
}

#' The columns and clusters of a chart, from its spec
#'
#' What checking an argument needs, without preparing the data: `columns` and
#' `clusters` are those of `prepare_data()`. After GC_transcript(), the clusters
#' are the transcripts, as in the chart GC_transcript() draws.
#'
#' @param GC_chart The gene chart object.
#' @return A list: the column names `start`, `end`, `cluster`, `group` and
#'   `strand` (NULL when not given), `columns`, the names of the columns of the
#'   prepared data, and `clusters`, the names of the clusters in order (none
#'   when the data has no rows).
#' @noRd
chart_fields <- function(GC_chart) {
  spec <- GC_chart$x$spec
  fields <- spec$fields
  state <- list(
    start = if (is.null(fields$start)) "start" else fields$start,
    end = if (is.null(fields$end)) "end" else fields$end,
    cluster = fields$cluster,
    group = fields$group,
    strand = fields$strand
  )
  if (!is.null(spec$transcript)) {
    first <- if (is_spec_entry(spec$transcript)) spec$transcript else spec$transcript[[1]]
    state$cluster <- if (is.null(first$transcript)) "transcript" else first$transcript
    state["strand"] <- list(first$strand)
  }
  data <- spec$data
  # prepare_data() leaves out the rows without a start or end
  kept <- !is.na(data[[state$start]]) & !is.na(data[[state$end]])
  # Also for data without rows: its columns can still be named (B75)
  state$columns <- union(names(data), c("rowID", "start", "end", "strand"))
  state$clusters <- if (!any(kept)) {
    character()
  } else if (is.null(state$cluster)) {
    "cluster"
  } else {
    unique(as.character(data[[state$cluster]][kept]))
  }
  state
}

#' `chart_fields()` and the prepared rows of a chart, as `data`
#'
#' For what works on the rows: links, introns, normalization, alignment and
#' editing single rows.
#' @noRd
chart_state <- function(GC_chart) {
  state <- chart_fields(GC_chart)
  state$data <- prepare_data(GC_chart$x$spec$data, state$start, state$end, state$cluster,
                             state$group, state$strand, warn = FALSE)
  state
}

#' The names of the clusters of prepared data
#' @noRd
state_clusters <- function(data, cluster) {
  if (nrow(data) == 0) return(character())
  if (is.null(cluster)) "cluster" else unique(as.character(data[[cluster]]))
}

#' The genes of one cluster, as the library splits the data
#'
#' The rows only: a `cluster` column is not added, as it would replace a
#' column of that name when the cluster field is another column.
#' @noRd
cluster_rows <- function(state, name) {
  if (is.null(state$cluster)) state$data else state$data[state$data[[state$cluster]] == name, ]
}

#' preRenderHook of GC_chart(): the spec as it is sent
#'
#' Leaves out the columns of sequences the chart does not draw, as R did
#' before the spec (decided 2026-11-05).
#' @noRd
send_spec <- function(widget) {
  widget$x$spec <- named_vectors_as_lists(drop_sequence_columns(widget$x$spec))
  widget
}

#' Named vectors of options as named lists
#'
#' A named vector, e.g. `GC_pattern(color = c(A = "white"))`, becomes a JSON
#' object as a named list does. jsonlite does this for named vectors only with
#' a deprecation message (`keep_vec_names`), so they are sent as lists. Data
#' frames are kept as they are.
#' @noRd
named_vectors_as_lists <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, named_vectors_as_lists)
    return(x)
  }
  if (is.atomic(x) && !is.null(names(x))) {
    return(as.list(x))
  }
  x
}

# Columns sent to the browser only when an option names them, as in a tooltip
# formatter "{translation}": they hold long sequences the chart does not draw
sequence_columns <- c("translation", "sequence")

#' The spec without the sequence columns of its data that no option names
#' @noRd
drop_sequence_columns <- function(spec) {
  data <- spec$data
  if (!is.data.frame(data)) {
    return(spec)
  }
  strings <- option_strings(spec[setdiff(names(spec), "data")])
  unused <- Filter(function(column) {
    !any(grepl(column, strings, fixed = TRUE))
  }, intersect(sequence_columns, names(data)))
  if (length(unused) > 0) {
    spec$data <- data[, setdiff(names(data), unused), drop = FALSE]
  }
  spec
}

#' All character values of the options, outside data frames
#' @noRd
option_strings <- function(x) {
  if (is.data.frame(x)) {
    return(character())
  }
  if (is.list(x)) {
    return(unlist(lapply(x, option_strings), use.names = FALSE))
  }
  if (is.character(x)) x else character()
}
