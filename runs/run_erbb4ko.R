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
mk1 <- list(
    main_path = "data/erbb4ko/mcf7_erbb4_ko_main_n1.xlsx",
    gi50_path = "data/erbb4ko/mcf7_erbb4_ko_gi50_n1.xlsx",
    plate_layout = "data/erbb4ko/erbb4ko_plate_layout.xlsx"
)

mcf_ctrl <- process_plates(list(mc1), assay_id = "MCF7-Ctrl")
mcf_ko <- process_plates(list(mk1), assay_id = "MCF7-KO")

save_results(plot_drc(list(mcf_ctrl, mcf_ko)), save_folder = "figures/erbb4ko", append_file_name = "MCF7_combined")
