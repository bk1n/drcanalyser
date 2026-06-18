# Gets stats from a model, i.e. IC50, GI50, etc. (stub: model stats are computed
# inline in plot_drc()). Kept internal and undocumented.
get_model_stats <- function(
  model,
  units = "uM",
  stat_type = "GI50"
) {
    # get gi50 / ic50
    return(stat)
}

#' Plot dose-response curves
#'
#' Returns a ggplot given processed plate data, with log concentration on the x
#' axis and normalised intensity on the y axis, plus derived ED10/50/90 stats.
#'
#' @param processed_plates List of `process_plates()` results (one per
#'   assay/condition).
#' @param xlabs X-axis label (concentration).
#' @param ylabs Y-axis label.
#' @param units One of `'uM'`, `'nM'`; defaults to micromolar. Set to `NULL`
#'   for no units.
#' @param plate_legend_name Controls the name of the legend for `assay_id`.
#' @param title Optional plot title.
#' @param plot_mean If `TRUE`, collapses replicates by concentration into a mean
#'   and plots standard-error bars.
#' @param label_halfmax If `TRUE`, annotates the top-right of the plot with each
#'   condition's half-maximal stat (IC50/GI50/GR50) and its 95% confidence
#'   interval, one line per condition coloured to match the legend.
#' @return A list with the ggplot `plot` and a `stats` data.frame.
#' @export
plot_drc <- function(processed_plates,
                     xlabs = "Concentration",
                     ylabs = "Normalised Viability (%)",
                     units = "\u00b5M",
                     plate_legend_name = "Assay ID",
                     title = NULL,
                     plot_mean = F,
                     label_halfmax = F) {
    # returns simulated preds corresponding to model curve
    get_model_curve_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        newdata_df <- data.frame(
            concs = 10^seq(log10(max(plate$CONCS, na.rm = T)), log10(min(plate$CONCS, na.rm = T)), length.out = 200)
        )
        preds <- as.data.frame(stats::predict(model, newdata = newdata_df, interval = "confidence"))
        preds$concs <- newdata_df$concs
        preds$assay_id <- unique(plate$ASSAY_ID)
        return(preds)
    }
    drcs <- lapply(processed_plates, get_model_curve_)
    drcs <- do.call(rbind, drcs)

    # get raw data points
    get_data_points_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        df <- data.frame(
            concs = model$data$CONCS,
            response = model$data[, 2],
            assay_id = unique(plate$ASSAY_ID)
        )
        return(df)
    }
    df_points <- lapply(processed_plates, get_data_points_)
    df_points <- do.call(rbind, df_points)

    # if plot_mean, convert any replicates by concentration into mean and plot SE bars
    if (plot_mean) {
        df_points <- df_points %>%
            dplyr::filter(!is.na(concs)) %>%
            dplyr::group_by(concs, assay_id) %>%
            dplyr::summarise(
                se = stats::sd(response) / sqrt(dplyr::n()),
                response = mean(response)
            )
    }

    # get GI50/IC50 stats from model
    get_model_stats_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        stat_type <- gsub("TRT_INTENSITY_", "", colnames(model$data[, 2, drop = F]))

        stats <- as.data.frame(drc::ED(model, respLev = c(10, 50, 90), type = "absolute", display = F))
        colnames(stats) <- c(stat_type, "std_error")
        stats$levels <- rownames(stats)
        stats$assay_id <- unique(plate$ASSAY_ID)
        stats$units <- units
        stats$lower_ci <- rep(NA, 3)
        stats$upper_ci <- rep(NA, 3)

        ci <- as.data.frame(confint(model, level = 0.95))
        stats[stats$levels == "e:1:50", "lower_ci"] <- ci["e:(Intercept)", "2.5 %"]
        stats[stats$levels == "e:1:50", "upper_ci"] <- ci["e:(Intercept)", "97.5 %"]

        # Use dynamic column name based on stat_type
        stats <- dplyr::relocate(stats, assay_id, levels, dplyr::all_of(stat_type), std_error, units)

        rownames(stats) <- NULL

        return(stats)
    }
    stats <- lapply(processed_plates, get_model_stats_)
    stats <- do.call(rbind, stats)

    # plot
    y_max <- max(c(df_points$response, df_points$response + df_points$se, 100, drcs$Upper, drcs$Prediction), na.rm = T)
    y_min <- min(c(df_points$response, df_points$response - df_points$se, drcs$Lower, drcs$Prediction), na.rm = T)

    xlabs <- if (!is.null(units)) paste0(xlabs, " (", units, ")") else xlabs

    g <- ggplot2::ggplot(
        df_points,
        ggplot2::aes(
            x = concs,
            y = response
        )
    ) +
        ggplot2::geom_point(ggplot2::aes(shape = assay_id, color = assay_id)) +
        ggplot2::geom_line(data = drcs, ggplot2::aes(x = concs, y = Prediction, color = assay_id), inherit.aes = F) +
        ggplot2::geom_ribbon(data = drcs, ggplot2::aes(x = concs, y = Prediction, fill = assay_id, ymin = Upper, ymax = Lower), alpha = 0.1, inherit.aes = T) +
        ggplot2::scale_x_continuous(trans = "log10") +
        ggplot2::scale_y_continuous(limits = c(y_min, y_max), breaks = seq(from = ceiling(y_min / 25) * 25, to = floor(y_max / 25) * 25, by = 25)) +
        ggplot2::theme_bw() +
        ggplot2::labs(
            x = xlabs,
            y = ylabs,
            color = plate_legend_name,
            fill = plate_legend_name,
            shape = plate_legend_name
        ) +
        ggplot2::ggtitle(title)

    g <- if (plot_mean) g + ggplot2::geom_errorbar(ggplot2::aes(ymin = response - se, ymax = response + se), width = .1, alpha = .5) else g

    if (label_halfmax) {
        halfmax <- stats[stats$levels == "e:1:50", ]

        # the half-maximal value column is named after the stat type (IC50/GI50/GR50)
        stat_type <- setdiff(
            colnames(halfmax),
            c("assay_id", "levels", "std_error", "units", "lower_ci", "upper_ci")
        )
        stat_prefix <- gsub("50$", "", stat_type)

        # reproduce ggplot's default discrete palette so labels match the legend
        assay_levels <- sort(unique(df_points$assay_id))
        palette <- scales::hue_pal()(length(assay_levels))
        names(palette) <- assay_levels

        # plotmath unit token (quoted string), or nothing when units are NULL
        unit_part <- if (!is.null(units)) paste0(' ~ "', units, '"') else ""

        # stack one right-aligned line per condition down from the top-right corner
        n <- nrow(halfmax)
        x_pos <- max(df_points$concs, na.rm = T)
        y_pos <- y_max - (seq_len(n) - 1) * (y_max - y_min) * 0.06

        for (i in seq_len(n)) {
            val <- signif(halfmax[i, stat_type], 2)
            lo <- signif(halfmax[i, "lower_ci"], 2)
            hi <- signif(halfmax[i, "upper_ci"], 2)

            # plotmath string parsed by annotate(parse = TRUE): e.g. IC[50]: 1.2 [0.85, 1.6] µM
            label <- sprintf(
                '%s[50] * ": " * %s ~ "[" * %s * ", " * %s * "]"%s',
                stat_prefix, val, lo, hi, unit_part
            )

            g <- g + ggplot2::annotate(
                "text",
                x = x_pos,
                y = y_pos[i],
                label = label,
                parse = TRUE,
                color = palette[[halfmax[i, "assay_id"]]],
                hjust = 1,
                size = 3
            )
        }
    }

    return(list(
        plot = g,
        stats = stats
    ))
}

#' Summarise a fitted model
#'
#' Processes a model to get statistics (AIC, MSE, etc.).
#'
#' @param model A fitted `drc::drm` model.
#' @return A list with the `model`, its `summary`, `aic`, `resid`, `ssr` and
#'   `mse`.
#' @export
process_model <- function(model) {
    summary <- summary(model)
    aic <- stats::AIC(model)
    resid <- stats::residuals(model)
    ssr <- sum(resid^2)
    mse <- 1 / length(resid) * ssr
    return(list(
        model = model,
        summary = summary,
        aic = aic,
        resid = resid,
        ssr = ssr,
        mse = mse
    ))
}

#' Save a plot and statistics to a folder
#'
#' @param plot_results Results from [plot_drc()] or a list containing `$plot`
#'   and `$stats` slots.
#' @param save_folder Folder to save results into.
#' @param append_file_name String to append to the end of the filename before
#'   the extension.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @return Invisibly `NULL`; writes an SVG plot and a CSV of stats to disk.
#' @export
save_results <- function(plot_results, save_folder, append_file_name = NULL, width = 5, height = 5) {
    plot <- plot_results$plot
    stats <- plot_results$stats

    append_file_name <- if (!is.null(append_file_name)) paste0("_", append_file_name) else NULL

    fig_path <- paste0(save_folder, "/drc_plot", append_file_name, ".svg")

    cat("Saving plot to", fig_path, "\n")
    ggplot2::ggsave(
        fig_path,
        plot,
        width = width,
        height = height,
        units = "in",
        dpi = 600
    )

    stat_path <- paste0(save_folder, "/drc_stats", append_file_name, ".csv")
    cat("Saving stats to", stat_path, "\n")
    utils::write.csv(stats, stat_path, row.names = F)
}
