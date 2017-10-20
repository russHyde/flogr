# `flogr`: A lightweight, functional, logging tool for use in `R`-based data-analysis pipelines

You're analysing data in `R`: this involves piping together a bunch of functions and writing a report.
At the end, you've got access to the input-to and the output-from that pipeline of functions.
So, you can't readily peak at what the data looked like during the intermediate processing steps anymore.
To add those intermediate results to your report you could either

1. add side-effects to the functions in your pipeline, eg to print/plot results out during processing;
1. use the %T>% pass-through pipe to print-out intermediate results without modifying your functions;
1. save intermediate results to variables and print/plot them out within your report
1. use `flog`

If your pipeline looks like this:
```
result <- input %>% f1 %>% f2 %>% f3
```
Then you can rewrite the same pipeline using `flogr` as follows:
```
result <- input %>% flog(modifiers = c(f1, f2, f3), null_logger) %>% get_dataset
```
but this _just_ gives you the same information.

Suppose you wanted to make a graph based on `f1(input)`, but otherwise discard the results of `f1(input)`
```
result_tuple <- input %>%
  flog(modifier = f1,
       logger = function(post) ggplot(post, aes(x = x, y = y)) + geom_line()
       ) %>%
  flog(c(f2, f3), null_logger)
  
result2 <- get_dataset(result_tuple) # identical to `result` above
```
Then you could print out that graph after the pipeline has ran, rather than printing it as a side-effect:
```
# print out the graph
get_logdata(result_tuple)[[1]]
```

That is, `flog` just provides a wrapper around functions, where you can save intermediate results /
figures and access them later, without having to save those things as a side-effect.

TODO: `flog_filter_df`
