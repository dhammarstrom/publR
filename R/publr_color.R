#' This function returns color palettes based on color brewer 
#' @param color_palette Name of color palette to return
#' @param display If TRUE, a plot of n palettes is shown for comparison
#' @param return_all If TRUE, a list of all palettes is return
#' @return A color palette of your chioce or a plot off all palettes or a list containing all palettes
#' @export
publr_color <- function(color_palette = "cpxq04", display = FALSE, return_all = FALSE){
        
        # palette coding
        
        #color blind safe
        #print friendly
        #photocopy
        #sequential, divergent, qualitative
        #number
        
        # c p p s 1
        # x x x d 1
        # x x x q 1
        
        colors <- list(cpxd01 = c("#8c510a","#d8b365","#f6e8c3","#c7eae5","#5ab4ac","#01665e"), # color blind safe, print friendly
                        cpxd02 = c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837"),
                        cpxd03 = c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac"),
                        cpxd04 = c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb","#4575b4"),
                     
                     # color blind safe
                     cxxd01 = c("#543005","#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e","#003c30"),
                     cxxd02 = c("#8e0152","#c51b7d","#de77ae","#f1b6da","#fde0ef","#f7f7f7","#e6f5d0","#b8e186","#7fbc41","#4d9221","#276419"),
                     cxxd03 = c("#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#f7f7f7","#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b"),
                     cxxd04 = c("#7f3b08","#b35806","#e08214","#fdb863","#fee0b6","#f7f7f7","#d8daeb","#b2abd2","#8073ac","#542788","#2d004b"),
                     cxxd05 = c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061"),
                     cxxd06 = c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"),
                     
                     # color blind safe sequential 
                     cxxs01 = c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#006d2c","#00441b"),
                     cxxs02 = c("#f7fcfd","#e0ecf4","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#810f7c","#4d004b"),
                     cxxs03 = c("#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#0868ac","#084081"),
                     cxxs04 = c("#fff7ec","#fee8c8","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#b30000","#7f0000"),
                     cxxs05 = c("#fff7fb","#ece7f2","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#045a8d","#023858"),
                     cxxs06 = c("#fff7fb","#ece2f0","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"),
                     cxxs07 = c("#f7f4f9","#e7e1ef","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f"),
                     cxxs08 = c("#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177","#49006a"),
                     cxxs09 = c("#ffffe5","#f7fcb9","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#006837","#004529"),
                     cxxs10 = c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"),
                     cxxs11 = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#993404","#662506"),
                     cxxs12 = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"),
                     
                     cxxs13 = c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"),
                     cxxs14 = c("#f7fcf5","#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b"),
                     cxxs15 = c("#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525","#000000"),
                     cxxs16 = c("#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"),
                     cxxs17 = c("#fcfbfd","#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"),
                     cxxs18 = c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"),
                     cxxs19 = c("#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525","#000000"),
                     
                     
                     # qualitative, color blind safe, print friendly
                     cpxq01 = c("#1b9e77","#d95f02","#7570b3"),
                     cpxq02 = c("#a6cee3","#1f78b4","#b2df8a"),
                     cpxq03 = c("#66c2a5","#fc8d62","#8da0cb"),
                     cpxq04 = c("#a6cee3","#1f78b4","#b2df8a","#33a02c"))
        
        
        if(display){
                
                dl <- data.frame(palette = rep(names(colors), sapply(colors, length)),
                                 color = unlist(colors, use.names = FALSE))
                
                dl <- cbind(dl, do.call("rbind", strsplit(as.character(dl$palette), "")))
                
                colnames(dl) <- c("palette", "color", "cb", "pr", "pc", "pa", "n1", "n2")
                
                dl$colorblind_friendly <- ifelse(dl$cb == "c", "colorblind ok", "colorblind problems")
                dl$printer_friendly <- ifelse(dl$pr == "p", "printer ok", "printer problems")
                dl$photocopy_friendly <- ifelse(dl$pc == "p", "photocopy ok", "photocopy problems")
                dl$type <- ifelse(dl$pa == "s", "sequential", ifelse(dl$pa == "d", "divergent", "qualitative"))
                
                dl <- dl[, c(1,2,9,10,11,12)]
                
                dl <- dplyr::mutate(dl, color_unique = paste0(palette, color)) 
                
                plot <- ggplot2::ggplot(dl, aes(palette, fill = color_unique)) + ggplot2::geom_bar() +
                        ggplot2::scale_fill_manual(values = as.character(dl$color))  +
                        ggplot2::theme_classic() +
                        ggplot2::coord_flip() + ggplot2::xlab("Palette name") +
                        ggplot2::facet_grid(.~printer_friendly) + ggplot2::theme(legend.position = "none")
                
                return(plot)
                
        }
        
        if(!(display) & !(return_all)){
             
                return(colors[[color_palette]])   
                
        }
        
        if(!(display) & return_all){
                
                return(colors)
        }
        
}


