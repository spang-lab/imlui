cli__visualize_txt <- "Usage:
    visualize [-h] -m <model> -d <dataset>
Options:
    -h, --help            show this help and exit
    -m, --model <model>   model to visualize
    -d, --data <dataset>  dataset to visualize"
cli__visualize <- function(argv) {
  trace_func_entry()
  args <- docopt::docopt(cli__visualize_txt, args = argv)
}

