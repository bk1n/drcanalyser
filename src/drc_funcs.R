library(drc)
library(tidyverse)

# Takes a standard format Excel output from SparkControl, and returns a df of the plate
# Plate format has columns 1:12, and rownames A:H
format_standard_xl <- function(path) {
    df <- readxl::read_excel(path, skip = 43, n_max = 10) %>%
        as.data.frame() %>%
        column_to_rownames(1)
    return(df)
}

# Define the V1 plate layout; see viability_assay_plate_layout.xlsx for more info on V1 layout.
# each list has columns and rows specified for layout
# columns should be integer ranges, rows should be vector or list of vectors
plate_layout_v1 <- function() {
    # n repeats per plate
    n_repeats <- 3
    n_steps <- 12

    # define plate layout
    # where a list is supplied for rows or columns, repeats are taken for each element in the list
    doses <- list(
        "columns" = 2:7,
        "rows" = list(c("B", "C", "D"), c("E", "F", "G"))
    )

    neg_ctrl <- list(
        "columns" = 8:9,
        "rows" = c("B", "C", "D", "E", "F", "G")
    )

    background <- list(
        "columns" = 10:11,
        "rows" = c("B", "C", "D", "E", "F", "G")
    )

    return(list(
        doses = doses,
        neg_ctrl = neg_ctrl,
        background = background
    ))
}

# Calculates serial dilution doses for a halflog dilution, with n_steps and a top_dose
get_halflog_doses <- function(top_dose = 30, n_steps = 0:11) {
    return(sapply(n_steps, function(n) top_dose / (10^(0.5 * n))))
}

# Given a plate and a plate layout this function will calculate average intensities for technical replicates, normalise intensities to background, and calculate percentage of maximum normalised response.
# Args:
# - plate: df of plate, 96well plates, with columns 1:12 and rownames A:H
# - plate_layout: named list with 'doses' = dosed wells, 'neg_ctrl' = negative control, and 'background' = background wells. See 'plate_layout_v1' for more details on required format.
# If lists are supplied in plate_layout$doses$columns or $rows, instead of vectors, then these will be assumed to be part of the same dose dilution sequence.
# Currently, doses are fetched from get_halflog_doses
# Returns a data.frame with mean intensities, intensities normalised to background, and intensities normalised to negative control (100% intensity).
process_plate <- function(plate, plate_layout) {
    # TODO - plate splitting only works for split rows currently, not columns
    # TODO - plate splitting assumes continuous ranges of rows, as in plate_layout_v1
    # TODO - change how doses are defined in dataframe - currently very static

    if (length(plate_layout$doses$rows) > 1) {
        split_plate <- lapply(plate_layout$doses$rows, function(rows) plate[rows, plate_layout$doses$columns])
        intensities <- do.call(cbind, split_plate)
    } else {
        intensities <- plate[plate_layout$doses$rows, plate_layout$doses$columns]
    }

    neg_ctrl <- mean(as.matrix(plate[plate_layout$neg_ctrl$rows, plate_layout$neg_ctrl$columns]))
    background <- mean(as.matrix(plate[plate_layout$background$rows, plate_layout$background$columns]))

    intensity_norm <- intensities - background
    intensity_mean <- colMeans(intensity_norm)
    intensity_se <- apply(intensity_norm, 2, function(col) sd(col) / sqrt(length(col)))
    intensity_norm_percmax <- (intensity_mean / neg_ctrl) * 100

    df <- data.frame(
        doses = get_halflog_doses(top_dose = 30, n_steps = 0:11),
        log_doses = log(get_halflog_doses(top_dose = 30, n_steps = 0:11)),
        norm_intensity_mean = intensity_mean,
        norm_intensity_se = intensity_se,
        norm_intensity_percmax = intensity_norm_percmax
    )
}

path <- "data/290524_n=2_EFO21.xlsx"
plate <- format_standard_xl(path)
print(plate)

efo1 <- process_plate(
    plate = format_standard_xl("data/270524_EFO21_n=1.xlsx"),
    plate_layout = plate_layout_v1()
)

efo2 <- process_plate(
    plate = format_standard_xl("data/290524_n=2_RMGI.xlsx"),
    plate_layout = plate_layout_v1()
)

res <- rbind(efo1, efo2)

plot(
    x = res$doses,
    y = res$norm_intensity_percmax,
    ylim = c(0, 100),
    log = "x"
)

model <- drm(norm_intensity_percmax ~ doses, data = res, fct = LL.4())

plot_drc <- function(processed_plate, model, xlab = 'dose', ylab = 'normalised response') {
    preds = as.data.frame(predict(model, newdata = processed_plate, interval = 'confidence'))
    
    df <- data.frame(
        dose = processed_plate$doses,
        response = processed_plate$norm_intensity_percmax,
        model = preds$Prediction,
        lwr = preds$Lower,
        upr = preds$Upper
    )

    y_max <- max(c(100, unlist(as.vector(df[,2:ncol(df)]))))
    y_min = min(c(0, unlist(as.vector(df[,2:ncol(df)]))))

    g = ggplot(
        df,
        aes(
            x = dose,
            y = response
        )
    ) +
        geom_point() +
        geom_line(aes(y = model), color = 'red') +
        geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.5, fill = 'lightblue') +
        scale_x_continuous(trans = "log10") +
        ylim(c(y_min, y_max)) +
        theme_bw() +
        labs(x = xlab,
        y = ylab)
    return(g)
}

plot_drc(res, model)
