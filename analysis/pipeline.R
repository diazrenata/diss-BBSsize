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

datasets <- datasets[1:10,]

set.seed(1977)

## a Drake plan that defines the methods
splist_methods <- drake::drake_plan(
    spec = target(list_species(bbs_dat),
                  transform = map(bbs_dat = !!rlang::syms(datasets$target))),
    all_spec = target(list(spec),
                      transform = combine(spec)),
    species_df = target(dplyr::bind_rows(all_spec)),
    distinct_df = target(dplyr::distinct(species_df)),
    saved_df = target(write.csv(distinct_df, here::here("analysis", "species_data", "species_list.csv"), row.names = F))
)

sizedat_methods <- drake::drake_plan(
    raw_size_dat = target(read.csv(here::here("analysis", "species_data", "species_list_working.csv"),  stringsAsFactors = F, strip.white = T, na.strings = "")),
    clean_size_dat = target(clean_sp_size_data(raw_size_dat)),
    fitted_pars = target(get_sd_parameters(raw_size_dat)),
    sd_size_dat = target(add_estimated_sds(clean_size_data = clean_size_dat,
                                           sd_pars = fitted_pars)),
    sp_mean_size_dat = target(get_sp_mean_size(sd_size_dat))
)

isd_methods <- drake::drake_plan(
    isd = target(simulate_size_dat(bbs_dat, mean_size_data = sp_mean_size_dat),
                 transform = map(bbs_dat = !!rlang::syms(datasets$target)))
)

isd_names <- as.character(unlist(isd_methods$target))

output_methods <- drake::drake_plan(
    saved_isd = target(save_isd(an_isd, a_save_name),
                       transform = map(an_isd = !!rlang::syms(isd_methods$target),
                                       a_save_name = !!isd_names)
    )
)
#
# ## The full workflow
# workflow <- dplyr::bind_rows(
#     datasets,
#     methods
# )

workflow <- dplyr::bind_rows(
    datasets,
    sizedat_methods,
    isd_methods,
    output_methods
)

## Visualize how the targets depend on one another
if (FALSE)
{
    config <- drake_config(workflow)
    sankey_drake_graph(config, build_times = "none", targets_only = TRUE)  # requires "networkD3" package
    vis_drake_graph(config, build_times = "none", targets_only = TRUE)     # requires "visNetwork" package
}

## Run the workflow
make(workflow)
