# Recover specie life strategy trait of Darling et al. (2019)

get_darling_trait <- function(final_impacts_global) {
  
  competitive <- c("Acropora abrolhosensis", "Acropora abrotanoides", "Acropora aculeus", "Acropora acuminata", "Acropora anthocercis", "Acropora aspera", "Acropora austera", "Acropora azurea", "Acropora carduus", "Acropora caroliniana", "Acropora cerealis", "Acropora chesterfieldensis", "Acropora clathrata", "Acropora corymbose,
  Acropora cytherea", "Acropora digitate", "Acropora digitifera", "Acropora divaricata", "Acropora donei", "Acropora echinata", "Acropora elseyi", "Acropora eurystoma", "Acropora florida", "Acropora gemmifera", "Acropora glauca",
  "Acropora globiceps", "Acropora grandis", "Acropora granulosa", "Acropora hispidose", "Acropora horrida",
  "Acropora humilis", "Acropora hyacinthus", "Acropora intermedia", "Acropora japonica", "Acropora kimbeensis",
  "Acropora latistella", "Acropora listeri", "Acropora lokani", "Acropora longicyathus", "Acropora loripes", "Acropora lutkeni", "Acropora microclados", "Acropora microphthalma", "Acropora millepora", "Acropora monticulosa",
  "Acropora muricata", "Acropora nana", "Acropora nasuta", "Acropora natalensis", "Acropora palmerae", "Acropora paniculata", "Acropora papillare", "Acropora pectinata", "Acropora polystoma", "Acropora pulchra", "Acropora retusa", "Acropora robusta", "Acropora roseni", "Acropora rudis", "Acropora samoensis", "Acropora sarmentosa",
  "Acropora secale", "Acropora selago", "Acropora solitaryensis", "Acropora speciosa", "Acropora spicifera",
  "Acropora striata", "Acropora subglabra", "Acropora subulata", "Acropora tenuis", "Acropora tortuosa", "Acropora valenciennesi", "Acropora valida", "Acropora vaughani", "Acropora verweyi", "Acropora willisae", "Acropora yongei", "Hydnophora rigida", "Isopora crateriformis", "Isopora cuneata", "Isopora elizabethensis", "Isopora palifera", "Montipora capitata", "Montipora digitata", "Montipora hispida", "Montipora incrassata", "Montipora mollis", "Montipora samarensis", "Montipora spongodes", "Montipora spumosa",
  "Montipora stellata", "Montipora turgescens", "Montipora undata")
  
  competitive <- gsub(" ", "_", competitive)
  
  generalist <- c("Cycloseris explanulata", "Cyphastrea agassizi", "Cyphastrea chalcidicum", "Cyphastrea decadia", "Cyphastrea japonica", "Cyphastrea microphthalma", "Cyphastrea ocellina", "Cyphastrea serailia", "Echinopora gemmacea",
  "Echinopora hirsutissima", "Echinopora horrida", "Echinopora lamellosa", "Echinopora mammiformis",
  "Echinopora pacificus", "Hydnophora exesa", "Hydnophora microconos", "Isopora cuneata", "Isopora palifera",
  "Leptastrea bewickensis", "Leptastrea inaequalis", "Leptastrea pruinosa", "Leptastrea purpurea", "Leptastrea transversa", "Merulina ampliata", "Merulina scabricula", "Montipora aequituberculata", "Montipora australiensis",
  "Montipora calcarea", "Montipora corbettensis", "Montipora crassituberculata", "Montipora danae", "Montipora florida", "Montipora foliosa", "Montipora foveolata", "Montipora grisea", "Montipora hoffmeisteri", "Montipora informis", "Montipora lobulata", "Montipora mactanensis", "Montipora monasteriata", "Montipora nodosa",
  "Montipora orientalis", "Montipora peltiformis", "Montipora tuberculosa", "Montipora turtlensis", "Montipora verrucosa", "Mycedium elephantotus", "Mycedium mancaoi", "Oxypora glabra", "Oxypora lacera", "Pachyseris rugosa", "Pachyseris speciosa", "Pavona bipartita", "Pavona cactus", "Pavona chiriquensis", "Pavona clavus",
  "Pavona decussata", "Pavona duerdeni", "Pavona explanulata", "Pavona frondifera", "Pavona maldivensis", "Pavona minuta", "Pavona varians", "Pavona venosa", "Pectinia alcicornis", "Pectinia lactuca", "Pectinia paeonia",
  "Pocillopora aliciae", "Pocillopora grandis", "Pocillopora ligulata", "Pocillopora meandrina", "Pocillopora verrucosa", "Pocillopora woodjonesi", "Podabacia crustacea", "Podabacia motuporensis", "Psammocora contigua",
  "Psammocora digitata", "Psammocora haimiana", "Psammocora nierstraszi", "Psammocora profundacella",
  "Psammocora stellata", "Turbinaria bifrons", "Turbinaria frondens", "Turbinaria heronensis",
  "Turbinaria mesenterina", "Turbinaria patula", "Turbinaria peltata", "Turbinaria radicalis",
  "Turbinaria reniformis", "Turbinaria stellulata")
  
  generalist <- gsub(" ", "_", generalist)
  
  stress_tol <- c("Acanthastrea echinata", "Acanthastrea hemprichii", "Acanthastrea pachysepta", "Alveopora allingi", "Alveopora tizardi", "Astrea annuligera", "Astrea curta", "Astreopora cucullata", "Astreopora listeri", "Astreopora myriophthalma", "Astreopora ocellata", "Astreopora randalli", "Astreopora scabra", "Australogyra zelli",
  "Bernardpora stutchburyi", "Blastomussa wellsi", "Caulastraea furcata", "Coelastrea aspera", "Coelastrea palauensis", "Coeloseris mayeri", "Coscinaraea columna", "Coscinaraea exesa", "Coscinaraea monile", "Ctenactis albitentaculata", "Cycloseris mokai", "Danafungia horrida", "Diploastrea heliopora", "Dipsastrea amicorum",
  "Dipsastrea danai", "Dipsastrea faviaformis", "Dipsastrea favus", "Dipsastrea laxa", "Dipsastrea lizardensis",
  "Dipsastrea matthaii", "Dipsastrea maxima", "Dipsastrea pallida", "Dipsastrea rotumana", "Dipsastrea rotundata",
  "Dipsastrea speciosa", "Dipsastrea veroni", "Dipsastrea vietnamensis", "Echinophyllia aspera", "Echinophyllia echinata", "Echinophyllia echinoporoides", "Echinophyllia orpheensis", "Euphyllia ancora", "Euphyllia divisa",
  "Euphyllia glabrescens", "Favites abdita", "Favites chinensis", "Favites complanata", "Favites flexuosa", "Favites halicora", "Favites magnistellata", "Favites pentagona", "Favites valenciennesi", "Favites vasta", "Fungia fungites",
  "Galaxea astreata", "Galaxea fascicularis", "Galaxea horrescens", "Gardineroseris planulata", "Goniastrea edwardsi", "Goniastrea favulus", "Goniastrea minuta", "Goniastrea pectinata", "Goniastrea retiformis", "Goniastrea stelligera", "Goniopora djiboutiensis", "Goniopora fruticosa", "Goniopora lobata", "Goniopora pedunculata",
  "Goniopora somaliensis", "Goniopora tenuidens", "Halomitra pileus", "Herpolitha limax", "Homophyllia bowerbanki", "Leptoria irregularis", "Leptoria phrygia", "Leptoseris explanata", "Leptoseris foliosa", "Leptoseris hawaiiensis", "Leptoseris incrustans", "Leptoseris mycetoseroides", "Leptoseris papyracea", "Leptoseris scabra",
  "Leptoseris solida", "Leptoseris yabei", "Lithophyllon concinna", "Lithophyllon repanda", "Lithophyllon scabra",
  "Lithophyllon undulatum", "Lobactis scutaria", "Lobophyllia agaricia", "Lobophyllia corymbosa", "Lobophyllia hataii", "Lobophyllia hemprichii", "Lobophyllia radians", "Lobophyllia recta", "Lobophyllia robusta", "Lobophyllia vitiensis", "Micromussa amakusensis", "Micromussa lordhowensis", "Micromussa regularis", "Montipora caliculata", "Montipora efflorescens", "Montipora flabellata", "Montipora floweri", "Montipora meandrina",
  "Montipora millepora", "Montipora patula", "Montipora venosa", "Oulophyllia crispa", "Paragoniastrea australensis", "Paragoniastrea russelli", "Paramontastrea annuligera", "Paramontastrea serageldini", "Physogyra lichtensteini", "Platygyra contorta", "Platygyra daedalea", "Platygyra lamellina", "Platygyra pini", "Platygyra sinensis", "Platygyra verweyi", "Platygyra yaeyamaensis", "Plerogyra sinuosa", "Plesiastrea versipora", "Pleuractis granulosa", "Pleuractis paumotensis", "Porites annae", "Porites arnaudi", "Porites australiensis", "Porites brighami",
  "Porites evermanni", "Porites gabonensis", "Porites lichen", "Porites lobata", "Porites lutea", "Porites mayeri", "Porites monticulosa", "Porites myrmidonensis", "Porites profundus", "Porites randalli", "Porites sillimaniani",
  "Porites stephensoni", "Stylocoeniella armata", "Stylocoeniella guentheri")
  
  stress_tol <- gsub(" ", "_", stress_tol)
  
  weedy <- c("Pocillopora damicornis", "Porites attenuata", "Porites compressa", "Porites cylindrica", "Porites heronensis",
  "Porites nigrescens", "Porites rus", "Porites vaughani", "Seriatopora caliendrum", "Seriatopora hystrix",
  "Seriatopora stellata", "Stylophora pistillata")
  
  weedy <- gsub(" ", "_", weedy)
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  final_impacts_global$life_trait <- NA
  
  final_impacts_global$life_trait[final_impacts_global$specie %in% competitive] <- "competitive" 
  final_impacts_global$life_trait[final_impacts_global$specie %in% generalist]  <- "generalist"
  final_impacts_global$life_trait[final_impacts_global$specie %in% stress_tol]  <- "stress_tol"
  final_impacts_global$life_trait[final_impacts_global$specie %in% weedy]       <- "weedy"
  
  final_impacts_trait <- final_impacts_global[!is.na(final_impacts_global$life_trait), ]
 
  final_impacts_trait
   
}
