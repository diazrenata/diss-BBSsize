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

which(grepl(datasets$target, pattern = "rtrg_102_18")) # hartland
which(grepl(datasets$target, pattern = "rtrg_304_17")) # comanche peak
which(grepl(datasets$target, pattern = "rtrg_133_6")) # portal BIRDS
which(grepl(datasets$target, pattern = "rtrg_19_35")) # wabash (salamonie state forest, indiana)
which(grepl(datasets$target, pattern = "rtrg_172_14")) # moraga (tilden)
which(grepl(datasets$target, pattern = "rtrg_113_25")) # island grove (micanopy/paynes)
which(grepl(datasets$target, pattern = "rtrg_63_25")) # ft mccoy (micanopy/paynes)
which(grepl(datasets$target, pattern = "rtrg_26_59")) # cranbury (princeton)



#datasets <- datasets[c(1:1000, 861, 1191, 1248, 1584, 1038, 1331, 1311, 2408),]
datasets <- datasets[c(1191, 1248, 1584, 1038, 1331, 1311, 2408),]
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

ibd_methods <- drake::drake_plan(
    ibd = target(make_ibd(isd),
                 transform = map(isd = !!rlang::syms(isd_methods$target)))
)

ibd_names <- as.character(unlist(ibd_methods$target))


new_dataset_methods <- drake::drake_plan(
    size = target(swap_currencies(orig_dat, ibd, currency = "mass"),
                  transform = map(orig_dat = !!rlang::syms(datasets$target),
                                  ibd = !!rlang::syms(ibd_names),
                                  .id = orig_dat)),
    energy = target(swap_currencies(orig_dat, ibd, currency = "energy"),
                    transform = map(orig_dat = !!rlang::syms(datasets$target),
                                    ibd = !!rlang::syms(ibd_names),
                                    .id = orig_dat))
)



newdat_names <- as.character(unlist(new_dataset_methods$target))

output_methods <- drake::drake_plan(
    saved_newdat = target(save_isd(a_dat, a_save_name),
                          transform = map(a_dat = !!rlang::syms(new_dataset_methods$target),
                                          a_save_name = !!newdat_names)
    ),
    saved_ibd = target(save_isd(a_dat, a_save_name),
                       transform = map(a_dat = !!rlang::syms(ibd_methods$target),
                                       a_save_name = !!ibd_names))
)

# ## The full workflow
# workflow <- dplyr::bind_rows(
#     datasets,
#     methods
# )

workflow <- dplyr::bind_rows(
    datasets,
    sizedat_methods,
    isd_methods,
    ibd_methods,
    new_dataset_methods,
    output_methods
)


## Run the workflow
make(workflow)

