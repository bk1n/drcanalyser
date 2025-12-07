source("src/drc_funcs.R")

OUT_PATH <- "figures/"

# EFO21
efo_1 <- list(
    main_path = "data/091024_base/091024_base_EFO21_main.xlsx",
    gi50_path = "data/091024_base/091024_base_EFO21_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)
efo_2 <- list(
    main_path = "data/290724_base/EFO21_n1_main.xlsx",
    gi50_path = "data/290724_base/EFO21_n1_GI50.xlsx",
    plate_layout = "data/290724_base/plate_layout_290724_n1.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)
efo_3 <- list(
    main_path = "data/290724_base/EFO21_n2_main.xlsx",
    gi50_path = "data/290724_base/EFO21_n2_GI50.xlsx",
    plate_layout = "data/290724_base/plate_layout_290724_n2.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)

efo <- process_plates(list(efo_1, efo_2, efo_3), assay_id = "EFO21", normalisation_method = "GR")

# RMGI
rmg_1 <- list(
    main_path = "data/091024_base/091024_base_RMGI_main.xlsx",
    gi50_path = "data/091024_base/091024_base_RMGI_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)
rmg_2 <- list(
    main_path = "data/290724_base/RMGI_n1_main.xlsx",
    gi50_path = "data/290724_base/RMGI_n1_GI50.xlsx",
    plate_layout = "data/290724_base/plate_layout_290724_n1.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)
rmg_3 <- list(
    main_path = "data/290724_base/RMGI_n2_main.xlsx",
    gi50_path = "data/290724_base/RMGI_n2_GI50.xlsx",
    plate_layout = "data/290724_base/plate_layout_290724_n1.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)

rmg <- process_plates(list(rmg_1, rmg_2, rmg_3), assay_id = "RMGI", normalisation_method = "GR")

save_results(plot_drc(list(efo, rmg)), save_folder = "figures/arid1ako_base", append_file_name = "combined")
save_results(plot_drc(list(efo, rmg), plot_mean = T), save_folder = "figures/arid1ako_base", append_file_name = "combined_mean")

cat("DONE\n")
