source("src/drc_funcs.R")

OUT_PATH <- "figures/"

plate_layout <- "data/290724_base/plate_layout_290724.xlsx"

# EFO21
efo1 <- process_plate(
    main_path = "data/290724_base/EFO21_n1_main.xlsx",
    gi50_path = "data/290724_base/EFO21_n1_GI50.xlsx",
    plate_layout = plate_layout
)
efo2 <- process_plate(
    main_path = "data/290724_base/EFO21_n2_main.xlsx",
    gi50_path = "data/290724_base/EFO21_n2_GI50.xlsx",
    plate_layout = plate_layout
)
efo3 <- process_plate(
    main_path = "data/early/270524_EFO21_n=1.xlsx",
    gi50_path = "data/290724_base/EFO21_n2_GI50.xlsx",
    plate_layout = plate_layout
)
res <- rbind(efo1, efo2, efo3)
model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model)

# RMGI
rmgi1 <- process_plate(
    main_path = "data/290724_base/RMGI_n1_main.xlsx",
    gi50_path = "data/290724_base/RMGI_n1_GI50.xlsx",
    plate_layout = plate_layout
)
rmgi2 <- process_plate(
    main_path = "data/290724_base/RMGI_n2_main.xlsx",
    gi50_path = "data/290724_base/RMGI_n2_GI50.xlsx",
    plate_layout = plate_layout
)
res <- rbind(rmgi1, rmgi2)
model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model)
