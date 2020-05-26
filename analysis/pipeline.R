library(MATSS)
library(drake)
library(BBSsize)

## include the functions in packages as dependencies
#  - this is to help Drake recognize that targets need to be rebuilt if the
#    functions have changed
expose_imports(MATSS)
expose_imports(BBSsize)

## a Drake plan for creating the datasets
#  - these are the default options, which don't include downloaded datasets
datasets <- build_bbs_datasets_plan()

#datasets <- datasets[1:100,]

## a Drake plan that defines the methods
methods <- drake::drake_plan(
    spec = target(list_species(bbs_dat),
                 transform = map(bbs_dat = !!rlang::syms(datasets$target))),
    all_spec = target(list(spec),
                     transform = combine(spec)),
    species_df = target(dplyr::bind_rows(all_spec)),
    distinct_df = target(dplyr::distinct(species_df)),
    saved_df = target(write.csv(distinct_df, here::here("analysis", "species_data", "species_list.csv"), row.names = F))
)


## The full workflow
workflow <- dplyr::bind_rows(
    datasets,
    methods
)

## Visualize how the targets depend on one another
if (interactive())
{
    config <- drake_config(workflow)
    sankey_drake_graph(config, build_times = "none", targets_only = TRUE)  # requires "networkD3" package
    vis_drake_graph(config, build_times = "none", targets_only = TRUE)     # requires "visNetwork" package
}

## Run the workflow
make(workflow)
