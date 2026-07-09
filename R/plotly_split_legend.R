#' Split the combined (colour, shape) ggplotly legend into two clean legends
#'
#' `plotly::ggplotly()` flattens a ggplot with both a colour and a shape
#' aesthetic into one trace per (colour, shape) combination and names the
#' legend entries as tuples such as `"(0,Sickerbox / Infiltration box)"` --
#' with two storage types and the 0..x / ">x" overflow palette that yields an
#' unreadable legend. This helper post-processes the plotly object:
#'
#' * the real traces lose their legend entries; instead every overflow class
#'   gets one legend-only key drawn as a **neutral circle in the class
#'   colour** (a coloured square or triangle would wrongly suggest one
#'   specific storage type). The key shares its legend group with the real
#'   traces of that class, so clicking it toggles **both** storage types of
#'   the class together;
#' * two legend-only keys (**neutral grey** filled square = infiltration box,
#'   filled triangle = gravel trench) are appended under their own
#'   **storage-type group title**, so the shape encoding is explained
#'   separately from the colours -- set `add_shape_legend = FALSE` to skip
#'   them (e.g. for storage-type-faceted plots whose strips already label the
#'   panels);
#' * the combined `"colour,shape"` legend-title annotation that ggplotly
#'   draws over the plot title is removed; group titles take its place and
#'   the legend moves to a vertical layout on the right, where the groups
#'   stack cleanly.
#'
#' Traces whose name is not a `"(colour,shape)"` tuple (frontier lines, best
#' markers, single-aesthetic plots) are left untouched, so the helper is safe
#' to apply to any of the package's interactive plots.
#'
#' @param pl A plotly object as returned by
#'   `plotly::ggplotly(p, tooltip = "text")`.
#' @param lang Character. `"de"` or `"en"`; sets the default legend group
#'   titles.
#' @param colour_title Character or `NULL`. Title of the colour legend group.
#'   Defaults to the language-specific "Number of overflow events".
#' @param shape_title Character or `NULL`. Title of the storage-type legend
#'   group. Defaults to the language-specific "Storage type".
#' @param add_shape_legend Logical. If `TRUE` (default), append the two
#'   legend-only storage-type entries.
#'
#' @return The modified plotly object.
#'
#' @examples
#' \dontrun{
#' p <- plot_cost_vs_evaporation(sim_results, param_grid, x = 5, lang = "de")
#' pl <- plotly::ggplotly(p, tooltip = "text")
#' pl <- plotly_split_legend(pl, lang = "de")
#' }
#'
#' @export
plotly_split_legend <- function(pl,
                                lang = c("de", "en"),
                                colour_title = NULL,
                                shape_title = NULL,
                                add_shape_legend = TRUE) {

  lang <- match.arg(lang)
  tt <- cost_tooltip_labels(lang)
  if (is.null(colour_title)) {
    colour_title <- switch(lang,
      de = "Anzahl \u00dcberlaufereignisse",
      en = "Number of overflow events")
  }
  if (is.null(shape_title)) shape_title <- tt$tt_storage_type

  traces <- pl$x$data
  tuple_re <- "^\\((.+?),(.+)\\)$"
  seen_colour <- character(0)
  colour_swatch <- list()
  shape_symbols <- list()
  found_tuples <- FALSE

  # Every real (colour, shape) trace loses its legend entry; the legend is
  # rebuilt from legend-only dummy traces below, so the colour keys can be
  # neutral circles and the storage-type keys neutral grey shapes -- a
  # coloured square/triangle key would wrongly suggest one specific
  # (colour, type) combination.
  for (i in seq_along(traces)) {
    nm <- traces[[i]]$name
    if (is.null(nm) || length(nm) != 1) next
    m <- regmatches(as.character(nm), regexec(tuple_re, as.character(nm)))[[1]]
    if (length(m) != 3) next
    found_tuples <- TRUE

    colour_lab <- trimws(m[2])
    shape_lab <- trimws(m[3])

    sym <- traces[[i]]$marker$symbol
    if (!is.null(sym) && length(sym) >= 1 && !shape_lab %in% names(shape_symbols)) {
      shape_symbols[[shape_lab]] <- sym[[1]]
    }
    col <- traces[[i]]$marker$color
    if (!is.null(col) && length(col) >= 1 && !colour_lab %in% names(colour_swatch)) {
      colour_swatch[[colour_lab]] <- col[[1]]
    }
    if (!colour_lab %in% seen_colour) seen_colour <- c(seen_colour, colour_lab)

    traces[[i]]$name <- colour_lab
    traces[[i]]$legendgroup <- colour_lab
    traces[[i]]$showlegend <- FALSE
  }

  # Layout clean-up shared by both cases: strip the legend-title annotation
  # that ggplotly draws over the plot title, convert two-line ggplot titles
  # ("\n") to plotly's "<br />", and move the legend to a vertical layout on
  # the right where nothing collides with the title.
  fix_layout <- function(pl) {
    ann <- pl$x$layout$annotations
    if (length(ann) > 0) {
      keep <- vapply(ann, function(a) {
        txt <- if (is.null(a$text)) "" else gsub("<[^>]+>", "", as.character(a$text))
        !(grepl(colour_title, txt, fixed = TRUE) ||
            grepl(shape_title, txt, fixed = TRUE))
      }, logical(1))
      pl$x$layout$annotations <- ann[keep]
    }
    if (!is.null(pl$x$layout$title$text)) {
      pl$x$layout$title$text <- gsub("\n", "<br />",
                                     pl$x$layout$title$text, fixed = TRUE)
    }
    pl$x$layout$showlegend <- TRUE
    pl$x$layout$legend$orientation <- "v"
    pl$x$layout$legend$x <- 1.02
    pl$x$layout$legend$xanchor <- "left"
    pl$x$layout$legend$y <- 1.0
    pl$x$layout$legend$yanchor <- "top"
    pl
  }

  # No combined-legend traces (single-aesthetic plot, e.g. the faceted design
  # space): keep the existing colour entries, but give the legend its title
  # (instead of the removed annotation) and order the classes numerically
  # with the ">x" catch-all last.
  if (!found_tuples) {
    for (i in seq_along(traces)) {
      nm <- traces[[i]]$name
      nm <- if (is.null(nm)) "" else as.character(nm)
      if (grepl("^[0-9]+$", nm)) {
        traces[[i]]$legendrank <- 100 + as.numeric(nm)
      } else if (grepl("^>", nm)) {
        traces[[i]]$legendrank <- 800
      }
    }
    pl$x$data <- traces
    pl$x$layout$legend$title <- list(text = colour_title)
    return(fix_layout(pl))
  }

  # Colour classes ordered numerically, the ">x" catch-all after them.
  colour_num <- suppressWarnings(as.numeric(seen_colour))
  colour_sorted <- c(seen_colour[!is.na(colour_num)][order(colour_num[!is.na(colour_num)])],
                     sort(seen_colour[is.na(colour_num)]))

  # Legend-only colour keys: neutral circles in the class colour. They share
  # the legendgroup with the real traces of that class, so clicking a key
  # still toggles both storage types of the class together.
  # Legend-only traces need one null data point (x/y = NA -> [null] in the
  # JSON): plotly.js does not create legend entries for traces whose data
  # arrays are completely empty.
  first_colour <- TRUE
  for (lab in colour_sorted) {
    tr <- list(
      x = NA_real_, y = NA_real_,
      type = "scatter", mode = "markers",
      marker = list(symbol = "circle",
                    color = colour_swatch[[lab]],
                    size = 10),
      name = lab,
      legendgroup = lab,
      legendrank = 100 + match(lab, colour_sorted),
      showlegend = TRUE,
      hoverinfo = "none"
    )
    if (first_colour) {
      tr$legendgrouptitle <- list(text = colour_title)
      first_colour <- FALSE
    }
    traces[[length(traces) + 1]] <- tr
  }

  # Legend-only storage-type keys: neutral grey square / triangle.
  if (isTRUE(add_shape_legend) && length(shape_symbols) > 0) {
    first_shape <- TRUE
    for (lab in names(shape_symbols)) {
      tr <- list(
        x = NA_real_, y = NA_real_,
        type = "scatter", mode = "markers",
        # "#666666" = R "grey40"; R colour names are not valid CSS for
        # plotly.js
        marker = list(symbol = shape_symbols[[lab]], color = "#666666",
                      size = 10),
        name = lab,
        legendgroup = "storage_type_legend",
        legendrank = 900 + match(lab, names(shape_symbols)),
        showlegend = TRUE,
        hoverinfo = "none"
      )
      if (first_shape) {
        tr$legendgrouptitle <- list(text = shape_title)
        first_shape <- FALSE
      }
      traces[[length(traces) + 1]] <- tr
    }
  }

  pl$x$data <- traces

  # Group titles replace the legend title in the rebuilt legend.
  pl$x$layout$legend$title <- list(text = "")

  fix_layout(pl)
}
