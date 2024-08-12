source("src/drc_funcs.R")

OUT_PATH <- "figures/"

# mcf7
mcf7 <- list(
    list(
        main_path = "data/ini_test/mcf7_main.xlsx",
        gi50_path = "data/ini_test/mcf7_gi50.xlsx",
        plate_layout = "data/ini_test/plate_layout_ini_test.xlsx"
    )
)
mcf_pp <- process_plates(path_list = mcf7, assay_id = "MCF7")
plot_drc(list(mcf_pp))
