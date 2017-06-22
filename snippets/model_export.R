# save model and export predictions
step_output_name <- get_step_output(args$pipeline, args$step)
step_output_name_hashed <- get_step_output_hashed(args$pipeline, args$step)
step_outputs <- list(model = get(paste0(step_def$model, "_base")), validate_error = validate_error, validate_predictions = validate_predictions, test_predictions = test_predictions)
save(step_outputs, file = sprintf("./data/models/hashed/%s.Rdata", step_output_name_hashed))
system(sprintf("ln -sr ./data/models/hashed/%s.Rdata ./data/models/%s.Rdata", step_output_name_hashed, step_output_name))
write.csv(test_predictions, sprintf("./data/test_predictions/%s.csv", step_output_name), row.names = FALSE)