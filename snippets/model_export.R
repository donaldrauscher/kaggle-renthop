# save model and export predictions
model_name <- paste(args$pipeline, args$step, args$univ, model_type, sep = "_")
assign(model_name, list(model = get(paste0(model_type, "_base")), validate_multiloss = validate_multiloss, validate_predictions = validate_predictions, test_predictions = test_predictions))
save(list = c(model_name), file = sprintf("./data/models/%s.Rdata", model_name))
write.csv(test_predictions, sprintf("./data/test_predictions/%s.csv", model_name), row.names = FALSE)