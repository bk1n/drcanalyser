source("src/drc_funcs.R")

OUT_PATH <- "figures/"

# EFO21
efo_ctrl_1 <- list(
    main_path = "data/091024_arid1ako/091024_EFO_Ctrl_main.xlsx",
    gi50_path = "data/091024_arid1ako/091024_EFO_Ctrl_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    )
)
# c("B", 3), c("C", 3), c("D", 3)
efo_ctrl_2 <- list(
    main_path = "data/050824_arid1ako/EFO21_Ctrl_n=1_main.xlsx",
    gi50_path = "data/050824_arid1ako/EFO21_Ctrl_n=1_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824_EFO_Ctrl.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)
efo_ctrl_3 <- list(
    main_path = "data/050824_arid1ako/EFO21_Ctrl_n=2_main.xlsx",
    gi50_path = "data/050824_arid1ako/EFO21_Ctrl_n=2_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824_EFO_Ctrl.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    ) # exclude top dose
)

efo_ko_1 <- list(
    main_path = "data/091024_arid1ako/091024_EFO_KO_main.xlsx",
    gi50_path = "data/091024_arid1ako/091024_EFO_KO_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2)
    )
)
efo_ko_2 <- list(
    main_path = "data/050824_arid1ako/EFO21_KO_n=1_main.xlsx",
    gi50_path = "data/050824_arid1ako/EFO21_KO_n=1_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(c("F", 6), c("B", 2), c("C", 2), c("D", 2))
)
efo_ko_3 <- list(
    main_path = "data/050824_arid1ako/EFO21_KO_n=2_main.xlsx",
    gi50_path = "data/050824_arid1ako/EFO21_KO_n=2_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(c("B", 2), c("C", 2), c("D", 2))
)

efo_ctrl <- process_plates(list(efo_ctrl_1, efo_ctrl_2, efo_ctrl_3), assay_id = "EFO21-Ctrl")

efo_ctrl_1 <- process_plates(list(efo_ctrl_1), assay_id = "EFO21-Ctrl")
efo_ctrl_2 <- process_plates(list(efo_ctrl_2), assay_id = "EFO21-Ctrl")
efo_ctrl_3 <- process_plates(list(efo_ctrl_3), assay_id = "EFO21-Ctrl")

save_results(plot_drc(list(efo_ctrl)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_Ctrl_combined")
save_results(plot_drc(list(efo_ctrl), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_Ctrl_combined_mean")
save_results(plot_drc(list(efo_ctrl_1)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_Ctrl_n1")
save_results(plot_drc(list(efo_ctrl_2)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_Ctrl_n2")
save_results(plot_drc(list(efo_ctrl_3)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_Ctrl_n3")

efo_ko <- process_plates(list(efo_ko_1, efo_ko_2, efo_ko_3), assay_id = "EFO21-KO")

efo_ko_1 <- process_plates(list(efo_ko_1), assay_id = "EFO21-KO")
efo_ko_2 <- process_plates(list(efo_ko_2), assay_id = "EFO21-KO")
efo_ko_3 <- process_plates(list(efo_ko_3), assay_id = "EFO21-KO")

save_results(plot_drc(list(efo_ko)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_KO_combined")
save_results(plot_drc(list(efo_ko), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_KO_combined_mean")
save_results(plot_drc(list(efo_ko_1)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_KO_n1")
save_results(plot_drc(list(efo_ko_2)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_KO_n2")
save_results(plot_drc(list(efo_ko_3)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_KO_n3")

save_results(plot_drc(list(efo_ko, efo_ctrl)), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_combined")
save_results(plot_drc(list(efo_ko, efo_ctrl), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "EFO21_combined_mean")

# RMGI
rmg_ctrl_1 <- list(
    main_path = "data/091024_arid1ako/091024_RMG_Ctrl_main.xlsx",
    gi50_path = "data/091024_arid1ako/091024_RMG_Ctrl_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3), # incorrectly dosed
        c("E", 2), c("F", 2), c("G", 2) # high var biological rep
    )
)
rmg_ctrl_2 <- list(
    main_path = "data/050824_arid1ako/RMGI_Ctrl_n=1_main.xlsx",
    gi50_path = "data/050824_arid1ako/RMGI_Ctrl_n=1_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3) # incorrectly dosed
    )
)
rmg_ctrl_3 <- list(
    main_path = "data/050824_arid1ako/RMGI_Ctrl_n=2_main.xlsx",
    gi50_path = "data/050824_arid1ako/RMGI_Ctrl_n=2_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3) # incorrectly dosed
    )
)

rmg_ko_1 <- list(
    main_path = "data/091024_arid1ako/091024_RMG_KO_main.xlsx",
    gi50_path = "data/091024_arid1ako/091024_RMG_KO_GI50.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3), # incorrectly dosed
        c("E", 2), c("F", 2), c("G", 2)
    )
)
rmg_ko_2 <- list(
    main_path = "data/050824_arid1ako/RMGI_KO_n=1_main.xlsx",
    gi50_path = "data/050824_arid1ako/RMGI_KO_n=1_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3) # incorrectly dosed
    )
)
rmg_ko_3 <- list(
    main_path = "data/050824_arid1ako/RMGI_KO_n=2_main.xlsx",
    gi50_path = "data/050824_arid1ako/RMGI_KO_n=2_GI50.xlsx",
    plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
    exclude = list(
        c("B", 2), c("C", 2), c("D", 2), # incorrectly dosed
        c("B", 3), c("C", 3), c("D", 3) # incorrectly dosed
    )
)

rmg_ctrl <- process_plates(list(rmg_ctrl_1, rmg_ctrl_2, rmg_ctrl_3), assay_id = "RMGI-Ctrl")

rmg_ctrl_1 <- process_plates(list(rmg_ctrl_1), assay_id = "RMGI-Ctrl")
rmg_ctrl_2 <- process_plates(list(rmg_ctrl_2), assay_id = "RMGI-Ctrl")
rmg_ctrl_3 <- process_plates(list(rmg_ctrl_3), assay_id = "RMGI-Ctrl")

save_results(plot_drc(list(rmg_ctrl)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_Ctrl_combined")
save_results(plot_drc(list(rmg_ctrl), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_Ctrl_combined_mean")
save_results(plot_drc(list(rmg_ctrl_1)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_Ctrl_n1")
save_results(plot_drc(list(rmg_ctrl_2)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_Ctrl_n2")
save_results(plot_drc(list(rmg_ctrl_3)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_Ctrl_n3")

rmg_ko <- process_plates(list(rmg_ko_1, rmg_ko_2, rmg_ko_3), assay_id = "RMGI-KO")

rmg_ko_1 <- process_plates(list(rmg_ko_1), assay_id = "RMGI-KO")
rmg_ko_2 <- process_plates(list(rmg_ko_2), assay_id = "RMGI-KO")
rmg_ko_3 <- process_plates(list(rmg_ko_3), assay_id = "RMGI-KO")

save_results(plot_drc(list(rmg_ko)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_KO_combined")
save_results(plot_drc(list(rmg_ko), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_KO_combined_mean")
save_results(plot_drc(list(rmg_ko_1)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_KO_n1")
save_results(plot_drc(list(rmg_ko_2)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_KO_n2")
save_results(plot_drc(list(rmg_ko_3)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_KO_n3")

save_results(plot_drc(list(rmg_ko, rmg_ctrl)), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_combined")
save_results(plot_drc(list(rmg_ko, rmg_ctrl), plot_mean = T), save_folder = "figures/091024_arid1ako", append_file_name = "RMGI_combined_mean")
