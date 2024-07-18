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

# Calculates serial dilution doses for a halflog dilution, with n_steps and a top_dose
get_halflog_doses <- function(top_dose = 30, n_steps = 0:11) {
    return(sapply(n_steps, function(n) top_dose / (10^(0.5 * n))))
}

# Checks validity of plate layout from spreadsheet
check_plate_layout <- function(layout) {
    mat <- as.matrix(layout)[, -1]
    colnames(mat) <- NULL
    rownames(mat) <- NULL

    contents <- table(mat)
    contents_vec <- as.vector(contents)
    names(contents_vec) <- names(contents)

    cat("Number of wells included in layout:\n")
    print(contents_vec)

    clean_contents_vec <- gsub("::n[0-9]", "", names(contents_vec))
    check <- c("bckgrnd", "neg_ctrl", "trt") %in% clean_contents_vec
    names(check) <- c("bckgrnd", "neg_ctrl", "trt")

    if (all(check)) {
        cat("All necessary elements found in layout\n")
        return(TRUE)
    } else {
        cat("Missing necessary elements in layout:\n")
        print(check)
        return(FALSE)
    }
}

# read in standardised layout spreadsheet from path
read_plate_layout <- function(path = "plate_layout.xlsx") {
    layout <- readxl::read_excel(path, sheet = "layout")
    concs <- readxl::read_excel(path, sheet = "concs")

    layout_df <- as.data.frame(layout)[, -1]
    rownames(layout_df) <- layout[[1]]

    stopifnot(check_plate_layout(layout_df))

    concs_df <- as.data.frame(concs)[, -1]
    rownames(concs_df) <- concs[[1]]

    return(list(layout_df, concs_df))
}

# mark concentrations for exclusion from data.frame
# exclude should be an integer or vector of integers, bottom dose is 1
# returns data.frame with exclude column containing 'exclude' flag
exclude_concentrations <- function(df, exclude) {
    # Ensure 'concs' column exists
    if (!"concs" %in% names(df)) {
        stop("Data frame must contain a 'concs' column.")
    }

    if (!is.null(exclude)) {
        df$exclude <- if_else(1:nrow(df) %in% exclude, "exclude", NA)
    } else {
        df$exclude <- NA
    }

    return(df)
}

# Normalise intensities from viability assay given plate_layout and path
# Args:
# - path: Excel spreadsheet generated from SparkControl, see format_standard_xl for details
# - plate_layout: optional, path to Excel spreadsheet with layout and concentrations, see read_plate_layout for details
# - exclude: optional, concentration ranges to be excluded, increasing order only, list of vectors (list(c(3,5), c(15,20)))
# Returns:
# - df: data.frame of concs, normalised treatment intensities
process_plate <- function(path, plate_layout = NULL, exclude = NULL) {
    r <- if (!is.null(plate_layout)) read_plate_layout(plate_layout) else read_plate_layout()

    layout <- r[[1]]
    concs <- r[[2]]
    intensity <- format_standard_xl(path)

    intensity_lst <- list()
    conc_lst <- list()
    for (i in 1:nrow(layout)) {
        for (j in 1:ncol(layout)) {
            l <- layout[i, j]
            c <- concs[i, j]
            intens <- intensity[i, j]

            intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
            if (grepl("trt", l)) {
                conc_lst[[l]] <- c(conc_lst[[l]], as.numeric(c))
            }
        }
    }

    # check all concentrations are identical for all repeats, merge
    all_ident <- all(sapply(conc_lst[-1], function(x) identical(conc_lst[[1]], x)))
    if (!all_ident) stop("Concentrations are not the same for all repeats, check layout.")
    concs_merged <- conc_lst[[1]]

    # take means of neg ctrls and backgrounds
    bckgrnd <- mean(intensity_lst[["bckgrnd"]])
    neg_ctrl <- mean(intensity_lst[["neg_ctrl"]]) - bckgrnd

    # normalise treatment intensities
    trt_int <- sapply(intensity_lst[grepl("trt", names(intensity_lst))], function(x) x - bckgrnd)
    trt_int_mean <- rowMeans(trt_int)
    trt_int_se <- apply(trt_int, 1, function(row) sd(row) / sqrt(length(row)))
    trt_int_norm_percmax <- (trt_int_mean / neg_ctrl) * 100

    df <- data.frame(
        concs = concs_merged,
        log_concs = log(concs_merged),
        trt_int_mean = trt_int_mean,
        trt_int_se = trt_int_se,
        trt_int_norm_percmax = trt_int_norm_percmax
    )

    df <- exclude_concentrations(df, exclude)

    return(df)
}

# Returns ggplot given a processed plate dataframe, with log conc on x and norm intensity on y
# Args:
# - processed_plate: data.frame from `process_plate()`
# - model: drc model object, for plotting model fit on ggplot
# - units: one of 'uM', 'nM', defaults to 'uM', set to NULL for no units
plot_drc <- function(
    processed_plate,
    model = NULL,
    exclude = FALSE,
    xlabs = "Concentration",
    units = "uM",
    ylabs = "Normalised Response (%)") {
    if (!is.null(model)) {
        newdata_df <- data.frame(
            concs = 10^seq(log10(max(processed_plate$concs)), log10(min(processed_plate$concs)), length.out = 200)
        )
        preds <- as.data.frame(predict(model, newdata = newdata_df, interval = "confidence"))
        preds$concs <- newdata_df$concs
    } else {
        newdata_df <- processed_plate
    }

    if (exclude) {
        processed_plate <- processed_plate[is.na(processed_plate$exclude), ]
    }

    df_points <- data.frame(
        concs = processed_plate$concs,
        response = processed_plate$trt_int_norm_percmax
    )

    df_drc <- data.frame(
        concs = preds$concs,
        preds = preds$Prediction,
        lwr = preds$Lower,
        upr = preds$Upper
    )

    y_max <- max(c(100, unlist(as.vector(df_drc))), na.rm = T)
    y_min <- min(c(0, unlist(as.vector(df_drc))), na.rm = T)

    xlabs <- if (!is.null(units)) paste0(xlabs, " (", units, ")") else xlabs

    g <- ggplot(
        df_points,
        aes(
            x = concs,
            y = response
        )
    ) +
        geom_point() +
        geom_line(data = df_drc, aes(x = concs, y = preds), color = "red", inherit.aes = F) +
        geom_ribbon(data = df_drc, aes(x = concs, y = preds, ymin = lwr, ymax = upr), alpha = 0.5, fill = "lightblue", inherit.aes = T) +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(from = ceiling(y_min / 25) * 25, to = floor(y_max / 25) * 25, by = 25)) +
        theme_bw() +
        labs(
            x = xlabs,
            y = ylabs
        )
    return(g)
}

# process model to get statistics (e.g. AIC, MSE) etc
process_model <- function(model) {
    summary <- summary(model)
    aic <- AIC(model)
    resid <- residuals(model)
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

# main ----

OUT_PATH <- "figures/"

# ## OVISE pre-test
# ovi <- process_plate(path = "data/270524_OVISE_n=1.xlsx")

# model <- drm(trt_int_norm_percmax ~ concs, data = ovi, fct = LL.4())
# processed_model <- process_model(model)

# plot_drc(
#     processed_plate = ovi,
#     model = processed_model$model,
#     exclude = TRUE
# )

# ## RMGI pre-test
# rmg <- process_plate(path = "data/270524_RMGI_n=1.xlsx")

# model <- drm(trt_int_norm_percmax ~ concs, data = rmg, fct = LL.4())
# processed_model <- process_model(model)

# plot_drc(rmg, processed_model$model, exclude = TRUE)

## EFO21 early 2024, eCF506, n=2
path <- "data/290524_n=2_EFO21.xlsx"
efo1 <- process_plate(path = path, exclude = 5)
path <- "data/270524_EFO21_n=1.xlsx"
efo2 <- process_plate(path = path, exclude = NULL)
res <- rbind(efo1, efo2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model, exclude = TRUE)

ggsave(paste0(OUT_PATH, "290524_EFO21.png"), g, width = 6, height = 6, units = "in")

## RMGI early 2024, eCF506, n=2
rmg1 <- process_plate(path = "data/270524_RMGI_n=1.xlsx")
rmg2 <- process_plate(path = "data/290524_n=2_RMGI.xlsx", exclude = 5)
res <- rbind(rmg1, rmg2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model, exclude = TRUE)

ggsave(paste0(OUT_PATH, "290524_RMGI.png"), g, width = 6, height = 6, units = "in")

## OVISE early 2024, eCF506, n=2
ovi1 <- process_plate(path = "data/270524_OVISE_n=1.xlsx")
ovi2 <- process_plate(path = "data/290524_n=2_OVISE.xlsx")

res <- rbind(ovi1, ovi2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
processed_model <- process_model(model)

g <- plot_drc(res, processed_model$model, exclude = TRUE)
ggsave(paste0(OUT_PATH, "290524_OVISE.png"), g, width = 6, height = 6, units = "in")
