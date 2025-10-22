# MCF7 ERBB4 KO vs Ctrl
# n=1 24/02/25
# n=2 03/03/25
# n=3 10/03/25

source("src/drc_funcs.R")

out_path <- "figures/"

mc1 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ctrl_main_n1.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ctrl_gi50_n1.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)
mc2 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ctrl_main_n2.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ctrl_gi50_n2.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)
mc3 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ctrl_main_n3.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ctrl_gi50_n3.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)
mk1 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ko_main_n1.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ko_gi50_n1.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)
mk2 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ko_main_n2.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ko_gi50_n2.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)
mk3 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ko_main_n3.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ko_gi50_n3.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)

mcf_ctrl <- process_plates(list(mc1, mc2, mc3), assay_id = "MCF7-Ctrl")
mcf_ko <- process_plates(list(mk1, mk2, mk3), assay_id = "MCF7-KO")

save_results(plot_drc(list(mcf_ctrl, mcf_ko), plot_mean = T), save_folder = "figures/erbb4ko", append_file_name = "MCF7_combined", width = 4, height = 3)
