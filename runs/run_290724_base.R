source("src/drc_funcs.R")

OUT_PATH <- "figures/"

plate_layout <- "data/290724_base/plate_layout_290724.xlsx"

# EFO21
efo <- list(
    list(
        main_path = "data/290724_base/EFO21_n1_main.xlsx",
        gi50_path = "data/290724_base/EFO21_n1_GI50.xlsx",
        plate_layout = plate_layout
    ),
    list(
        main_path = "data/290724_base/EFO21_n2_main.xlsx",
        gi50_path = "data/290724_base/EFO21_n2_GI50.xlsx",
        plate_layout = plate_layout
    )
)
efo_pp <- process_plates(efo, assay_id = "EFO21")

# RMGI
rmg <- list(
    list(
        main_path = "data/290724_base/RMGI_n1_main.xlsx",
        gi50_path = "data/290724_base/RMGI_n1_GI50.xlsx",
        plate_layout = plate_layout
    ),
    list(
        main_path = "data/290724_base/RMGI_n2_main.xlsx",
        gi50_path = "data/290724_base/RMGI_n2_GI50.xlsx",
        plate_layout = plate_layout
    )
)
rmg_pp <- process_plates(rmg, assay_id = "RMGI")

# plot all
processed_plates <- list(efo_pp, rmg_pp)
g <- plot_drc(processed_plates, ylabs = "Growth Inhibition (%)")

# save results
save_results(g, save_folder = "figures/290724_base")
