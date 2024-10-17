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

efo <- process_plates(list(efo_1, efo_2, efo_3), assay_id = "EFO21")

efo_1 <- process_plates(list(efo_1), assay_id = "EFO21")
efo_2 <- process_plates(list(efo_2), assay_id = "EFO21")
efo_3 <- process_plates(list(efo_3), assay_id = "EFO21")

save_results(plot_drc(list(efo)), save_folder = "figures/091024_base", append_file_name = "EFO21_combined")
save_results(plot_drc(list(efo), plot_mean = T), save_folder = "figures/091024_base", append_file_name = "EFO21_combined_mean")
save_results(plot_drc(list(efo_1)), save_folder = "figures/091024_base", append_file_name = "EFO21_n1")
save_results(plot_drc(list(efo_2)), save_folder = "figures/091024_base", append_file_name = "EFO21_n2")
save_results(plot_drc(list(efo_3)), save_folder = "figures/091024_base", append_file_name = "EFO21_n3")

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

rmg <- process_plates(list(rmg_1, rmg_2, rmg_3), assay_id = "RMGI")

rmg_1 <- process_plates(list(rmg_1), assay_id = "RMGI")
rmg_2 <- process_plates(list(rmg_2), assay_id = "RMGI")
rmg_3 <- process_plates(list(rmg_3), assay_id = "RMGI")

save_results(plot_drc(list(rmg)), save_folder = "figures/091024_base", append_file_name = "RMGI_combined")
save_results(plot_drc(list(rmg), plot_mean = T), save_folder = "figures/091024_base", append_file_name = "RMGI_combined_mean")
save_results(plot_drc(list(rmg_1)), save_folder = "figures/091024_base", append_file_name = "RMGI_n1")
save_results(plot_drc(list(rmg_2)), save_folder = "figures/091024_base", append_file_name = "RMGI_n2")
save_results(plot_drc(list(rmg_3)), save_folder = "figures/091024_base", append_file_name = "RMGI_n3")
