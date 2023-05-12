make_taxonomy <- function(coral_traits_final, habitat_sp, corals_range) {
  
  message(cli::rule(line_col = "blue", right = "Making the project WoRMS taxonomy"))
  
  databases <-  list(ctdb = targets::tar_read(coral_traits_final), iucn_habitat = targets::tar_read(habitat_sp),
                     corals_range = targets::tar_read(corals_range))
  
  # all species names
  sp_names <- lapply(names(databases), function(n) {
    switch(n,
           ctdb = databases$ctdb$specie_name,
           iucn_habitat = names(databases$iucn_habitat),
           corals_range = corals_range$sci_name
           )})

  names(sp_names) <- names(databases)
  genus_sp <- unique(unlist(sp_names))
  genus_sp <- genus_sp[order(genus_sp)]
  
  # creating a data frame with presence/absence of species in each database (1)
  sp_dbs <- as.data.frame(cbind(genus_sp, sapply(sp_names, function(i) {genus_sp %in% i})))
  
  # getting WORMS records
  gwr <- function(g_s) {
    gs <- gsub("_", " ", g_s)
    cat(gs, "\n")
    r <- tryCatch(worrms::wm_records_name(gs), error = function(e) NULL)
    if (is.null(r))
      r <- tryCatch(worrms::wm_records_taxamatch(gs)[[1]], error = function(e) NULL)
    if (is.null(r)) return(NULL)
    data.frame(r)
  }#eo gwr
  
  up_name <- function(g_s) {
    
    splitsubsp <- function(pat, g_s) sub(pat, paste0(pat, "_"), g_s)
    
    if (grepl("Anthemiphyllia_patera", g_s)) return(splitsubsp("Anthemiphyllia_patera", g_s))
    if (grepl("Balanophyllia", g_s)) return(gsub("_", "_(Balanophyllia)_", g_s))
    if (grepl("Caryophyllia_ambrosia", g_s)) return(splitsubsp("Caryophyllia_ambrosia", g_s))
    if (g_s == "Caryophyllia_cincticulatus") return("Caryophyllia_cintinculata")
    if (g_s == "Culicia_tenellanatalensis") return("Culicia_tenella_var._natalensis")
    if (g_s == "Culicia_tenellatenella") return("Culicia_tenella_tenella")
    # if (g_s == "Dipsastrea_faviaformis") return("Dipsastraea_faviaformis") #doing it in data_ctdb
    if (grepl("Flabellum_apertum", g_s)) return(
      gsub(" ", "_", sub("apertum", "apertum ", gsub("_", " (Ulocyathus) ", g_s))))
    if (grepl("Flabellum_transversale", g_s)) return(
      sub("transversale", "transversale_", gsub("_", "_(Flabellum)_", g_s)))
    if (grepl("Fungiacyathus", g_s)) g_s <- gsub("_", "_(Fungiacyathus)_", g_s)
    if (g_s == "Fungiacyathus_(Fungiacyathus)_pusilluspusillus") return("Fungiacyathus_(Fungiacyathus)_pusillus_pusillus")
    if (g_s == "Fungiacyathus_(Fungiacyathus)_pusilluspacificus") return("Fungiacyathus_(Fungiacyathus)_pusillus_pacificus")
    if (g_s == "Homophyllia_hillae") return("Acanthastrea_hillae")
    if (grepl("Oulangia_stokesiana", g_s) & (g_s != "Oulangia_stokesiana"))
      return(sub("Oulangia_stokesiana", "Oulangia_stokesiana_", g_s))
    if (g_s == "Paracyathus_indicusgracilis") return("Paracyathus_indicus_var._gracilis")
    if (g_s == "Paracyathus_indicusindicus") return("Paracyathus_indicus_var._indicus")
    if (grepl("Phyllangia_americana", g_s) & (g_s != "Phyllangia_americana")) return(splitsubsp("Phyllangia_americana", g_s))
    if (grepl("Isopora", g_s)) return(sub("Isopora", "Acropora", g_s))
    if (g_s == "Lithophyllon_puishani") return("Fungia_puishani")
    
    g_s
  }#eo up_name
  
  message(cli::rule(line_col = "yellow", right = "Obtaining worms records"))
  
  upnames   <- sapply(genus_sp, up_name)
  wr <- setNames(lapply(upnames, gwr), genus_sp)
  
  #filter worms records (from taxize::get_wormsid)
  filter_wr <- function(gs) {
    
    message(gs)
    #gs="Homophyllia hillae"
    sci_com <- up_name(gs)
    wmdf <- wr[[gs]]
    
    if (nrow(wmdf) == 1) return(wmdf)
    
    if (nrow(wmdf) > 1) {
      
      matchtmp <- wmdf[wmdf$status == "accepted", ]
      if (nrow(matchtmp) == 1) return(matchtmp)
      
      matchtmp <- wmdf[tolower(wmdf$scientificname) %in% tolower(gsub("_", " ", sci_com)), ]
      if (nrow(matchtmp) == 1) return(matchtmp)
      
      return(matchtmp[1,])
      
      stop(gs,"\n")
    }
    
  }#eo filter_wr
  
  wrf    <- do.call(rbind, parallel::mclapply(genus_sp, filter_wr))
  taxons <- data.frame(sp_dbs, updated_genus_sp = sapply(genus_sp, up_name), wrf)
  names(taxons) <- tolower(names(taxons))
  names(taxons)[names(taxons) == "aphiaid"] <- "aphia_id"
  names(taxons)[names(taxons) == "valid_aphiaid"] <- "valid_aphia_id"
  
  get_aphia <- function(x) setNames(ifelse(is.na(taxons[x, "valid_aphia_id"]),
                                           taxons[x, "aphia_id"],
                                           taxons[x, "valid_aphia_id"]), x)
  
  taxons$final_aphia_id <- sapply(taxons$genus_sp, get_aphia)
  
  taxons <- taxons[,c("genus_sp", "ctdb", "iucn_habitat", "corals_range",
                      "updated_genus_sp", "scientificname", "authority", "status", "unacceptreason",
                      "rank", "aphia_id", "valid_aphia_id", "final_aphia_id")]
  
  final_wr <- data.frame(do.call(rbind, lapply(taxons$final_aphia_id, function(id) {
    #message("ID : ", id)
    worrms::wm_record(id)})))
  
  if (!identical(taxons$final_aphia_id, final_wr$AphiaID)) stop("could not get some worms records for final_aphia_ids")
  
  names(final_wr) <- tolower(names(final_wr))
  final_wr <- final_wr[, c("kingdom", "phylum", "class", "order", "family", "genus",
                           "valid_name", "valid_authority", "citation")]
  names(final_wr)[names(final_wr) == "valid_name"] <- "genus_sp"
  names(final_wr) <- paste0("final_", names(final_wr))
  final_wr$final_genus_sp <- gsub(" ", "_", final_wr$final_genus_sp)
  
  taxons <- cbind(taxons, final_wr)
  names(taxons)[names(taxons) == "scientificname"] <- "worms_genus_sp"
  taxons$worms_genus_sp <- gsub(" ", "_", taxons$worms_genus_sp)
  
  taxons
  
}
