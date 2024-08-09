source("src/drc_funcs.R")

OUT_PATH <- "figures/"

## EFO21 early 2024, eCF506, n=2
path <- "data/290524_n=2_EFO21.xlsx"
efo1 <- process_plate(main_path = path, exclude = 5)
path <- "data/270524_EFO21_n=1.xlsx"
efo2 <- process_plate(main_path = path, exclude = NULL)
res <- rbind(efo1, efo2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model, exclude = TRUE)

ggsave(paste0(OUT_PATH, "290524_EFO21.png"), g, width = 6, height = 6, units = "in")

## RMGI early 2024, eCF506, n=2
rmg1 <- process_plate(main_path = "data/270524_RMGI_n=1.xlsx")
rmg2 <- process_plate(main_path = "data/290524_n=2_RMGI.xlsx", exclude = 5)
res <- rbind(rmg1, rmg2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
g <- plot_drc(res, model, exclude = TRUE)

ggsave(paste0(OUT_PATH, "290524_RMGI.png"), g, width = 6, height = 6, units = "in")

## OVISE early 2024, eCF506, n=2
ovi1 <- process_plate(main_path = "data/270524_OVISE_n=1.xlsx")
ovi2 <- process_plate(main_path = "data/290524_n=2_OVISE.xlsx")

res <- rbind(ovi1, ovi2)

model <- drm(trt_int_norm_percmax ~ concs, data = res, fct = LL.4())
processed_model <- process_model(model)

g <- plot_drc(res, processed_model$model, exclude = TRUE)
ggsave(paste0(OUT_PATH, "290524_OVISE.png"), g, width = 6, height = 6, units = "in")
