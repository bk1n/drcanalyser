source("src/drc_funcs.R")

OUT_PATH <- "figures/"

# EFO21
efo_ctrl <- list(
    list(
        main_path = "data/050824_arid1ako/EFO21_Ctrl_n=1_main.xlsx",
        gi50_path = "data/050824_arid1ako/EFO21_Ctrl_n=1_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824_EFO_Ctrl.xlsx",
        exclude = list(c("B", 2), c("C", 2), c("D", 2)) # exclude top dose
    ),
    list(
        main_path = "data/050824_arid1ako/EFO21_Ctrl_n=2_main.xlsx",
        gi50_path = "data/050824_arid1ako/EFO21_Ctrl_n=2_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824_EFO_Ctrl.xlsx",
        exclude = list(c("B", 2), c("C", 2), c("D", 2)) # exclude top dose
    )
)
efo_ko <- list(
    list(
        main_path = "data/050824_arid1ako/EFO21_KO_n=1_main.xlsx",
        gi50_path = "data/050824_arid1ako/EFO21_KO_n=1_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
        exclude = list(c("F", 6), c("C", 2))
    ),
    list(
        main_path = "data/050824_arid1ako/EFO21_KO_n=2_main.xlsx",
        gi50_path = "data/050824_arid1ako/EFO21_KO_n=2_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx",
        exclude = list(c("D", 2))
    )
)

efo_ctrl <- process_plates(efo_ctrl, assay_id = "EFO21-Ctrl")
efo_ko <- process_plates(efo_ko, assay_id = "EFO21-KO")

processed_plates <- list(efo_ctrl, efo_ko)
plt <- plot_drc(processed_plates)
plt$plot
plt$stats


# RMGI
rmg_ctrl <- list(
    list(
        main_path = "data/050824_arid1ako/RMGI_Ctrl_n=1_main.xlsx",
        gi50_path = "data/050824_arid1ako/RMGI_Ctrl_n=1_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx"
    ),
    list(
        main_path = "data/050824_arid1ako/RMGI_Ctrl_n=2_main.xlsx",
        gi50_path = "data/050824_arid1ako/RMGI_Ctrl_n=2_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx"
    )
)
rmg_ko <- list(
    list(
        main_path = "data/050824_arid1ako/RMGI_KO_n=1_main.xlsx",
        gi50_path = "data/050824_arid1ako/RMGI_KO_n=1_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx"
    ),
    list(
        main_path = "data/050824_arid1ako/RMGI_KO_n=2_main.xlsx",
        gi50_path = "data/050824_arid1ako/RMGI_KO_n=2_GI50.xlsx",
        plate_layout = "data/050824_arid1ako/plate_layout_050824.xlsx"
    )
)

rmg_ctrl <- process_plates(rmg_ctrl, assay_id = "RMGI-Ctrl")
rmg_ko <- process_plates(rmg_ko, assay_id = "RMGI-KO")

processed_plates <- list(rmg_ctrl, rmg_ko)
plt <- plot_drc(processed_plates)
plt$plot



# save results
save_results(g, save_folder = "figures/290724_base")
