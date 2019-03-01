#' Sépare une colonne de parcours en plusieurs colonnes
#'
#' Sépare une colonne de parcours en plusieurs colonnes.
#'
#' @param data Un dataframe avec les parcours et comptages associés.
#' @param path Une chaîne de caractères. Le nom de la colonne contenant les
#'   parcours.
#' @param count Une chaîne de caractères. Le nom de la colonne contenant les
#'   comptages.
#' @param sep Une chaîne de caractères (par défaut = "-"). Le séparateur
#'   utilisé dans la colonne 'path'.
#' @param prefix Une chaîne de caractères (par défaut = "depth"). Le préfixe
#'   utilisé pour le nom des colonnes contenant les éléments des parcours.
#'
#' @return Un dataframe où chaque élément d'un parcours est dans une colonne
#'   spécifique.
split_col <- function(data, path, count, sep = "-", prefix = "depth") {
  stopifnot(c(path, count) %in% names(data))

  split_ <- strsplit(data[[path]], split = sep, fixed = TRUE)

  max_length <- max(lengths(split_))

  df <- data.frame(
    t(
      sapply(split_, "[", i = seq_len(max_length))
    ),
    count = data[[count]],
    stringsAsFactors = FALSE
  )
  names(df)[1L:max_length] <- paste0(prefix, "_", seq_len(max_length))
  return(df)
}

#' Conserve l'ensemble des parcours d'une profondeur donnée
#'
#' Conserve l'ensemble des parcours d'une profondeur donnée.
#'
#' @param data Un dataframe où chaque élément d'un parcours est stocké dans une
#'   colonne distincte.
#' @param depth Un entier (par défaut = NULL). La profondeur des parcours à
#'   considérer. Si NULL, les données sont laissées comme telles.
#' @param prefix Une chaîne de caractères (par défaut = "depth"). Le préfixe
#'   utilisé pour le nom des colonnes contenant les éléments des parcours.
#' @param fill Une chaîne de caractères (par défaut = " "). La valeur utilisée
#'   pour remplacer les valeurs manquantes.
#'
#' @return Un dataframe.
keep_paths <- function(data, depth = NULL, prefix = "depth", fill = " ") {
  if (!is.null(depth)) {
    cols <- paste0(prefix, "_", seq_len(depth + 1L))

    stopifnot(cols %in% names(data))

    data[, cols] <- lapply(cols, function(col) {
      data[is.na(data[[col]]), col] <- fill
      return(data[[col]])
    })
  }
  return(data)
}

#' Agrège les couples "source" - "cible" pour un niveau de profondeur donné
#'
#' Agrège les couples "source" - "cible" pour un niveau de profondeur donné.
#'
#' @param data Un dataframe où chaque élément d'un parcours est stocké dans
#'   une colonne distincte.
#' @param depth Un entier. Le niveau de profondeur des parcours à considérer.
#' @param count Une chaîne de caractères, le nom de la colonne contenant le
#'   comptage des parcours.
#' @param prefix Une chaîne de caractères (par défaut = "depth"). Le préfixe
#'   utilisé pour le nom des colonnes contenant les éléments des parcours.
#'
#' @return Un dataframe avec l'interaction source, l'interaction cible ainsi que
#'   le comptage et le niveau de profondeur associés.
count_paths <- function(data, depth, count = "count", prefix = "depth") {
  var1 <- paste0(prefix, "_", depth)
  var2 <- paste0(prefix, "_", depth + 1L)

  count_ <- stats::aggregate(data[[count]],
                             by = list(data[[var1]], data[[var2]]),
                             FUN = sum, na.rm = TRUE)
  names(count_) <- c("source", "target", "count")
  count_[["depth"]] <- depth
  return(count_)
}

#' Crée un dataframe sous la forme interaction "source", interaction "cible",
#' comptage et niveau de profondeur associés
#'
#' Crée un dataframe sous la forme interaction "source", interaction "cible",
#' comptage et niveau de profondeur associés.
#'
#' @param data Un dataframe avec les parcours et comptages associés. Les
#'   parcours peuvent être dans une colonne unique ou bien dans des colonnes
#'   distinctes.
#' @param cols Un vecteur de chaînes de caractères (par défaut = NULL). Les
#'   colonnes contenant les éléments des parcours individuels. Le nom de ces
#'   colonnes doit être uniformisé sous la forme <prefix>_<niveau_de_profondeur>
#'   (par exemple "depth_1", "depth_2", etc.). Utiliser la valeur NULL si les
#'   parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours individuels sont contenus dans une seule colonne
#'   qui doit être séparée. Utiliser la valeur NULL si les éléments des parcours
#'   sont dans des colonnes distinctes.
#' @param count Une chaîne de caractères (par défaut = "count"). Le nom de la
#'   colonne contenant les comptages.
#' @param sep Une chaîne de caractères (par défaut = "-"). Le séparateur utilisé
#'   dans la colonne path.
#' @param depth Un entier (par défaut = NULL). Conserve l'ensemble des parcours
#'   d'une profondeur donnée. Si NULL, les données sont laissées comme telles.
#' @param fill Une chaîne de caractères (par défaut = " "). Si l'argument depth
#'   est utilisé, permet de spécifier la valeur remplacant les valeurs
#'   manquantes.
#'
#' @return Un dataframe contenant l'interaction "source", l'interaction "cible",
#'   le comptage et le niveau de profondeur associés.
#' @export
from_aggregated_paths <- function(data, cols = NULL, path = NULL,
                                  count = "count", sep = "-",
                                  depth = NULL, fill = " ") {
  if (is.null(cols) && is.null(path)) {
    stop("You have to use one of the 'cols' or 'path' argument.")
  }
  if (!is.null(cols) && !is.null(path)) {
    stop("You cannot use the 'cols' and the 'path' argument at the same time")
  }

  if (is.null(cols)) {
    data <- split_col(data = data, path = path, count = count, sep = sep)
    count <- "count"
    prefix_ <- "depth"
  } else {
    prefix_ <- gsub(pattern = "(.+)_[0-9]+", replacement = "\\1", cols[1])
  }

  data <- keep_paths(data = data, depth = depth, prefix = prefix_, fill = fill)

  length_ <- ncol(data) - 2L
  data <- lapply(seq_len(length_), count_paths, data = data,
                 count = count, prefix = prefix_)

  data <- do.call("rbind", data)
}

#' Agrège des parcours individuels
#'
#' Agrège des parcours individuels.
#'
#' @param data Un dataframe avec des parcours par individu.
#' @param cols Un vecteur de chaînes de caractères (par défaut = NULL). Les
#'   colonnes contenant les éléments des parcours individuels. Le nom de ces
#'   colonnes doit être uniformisé sous la forme <prefix>_<niveau_de_profondeur>
#'   (par exemple "depth_1", "depth_2", etc.). Utiliser la valeur NULL si les
#'   parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours individuels sont contenus dans une seule colonne
#'   qui doit être séparée. Utiliser la valeur NULL si les éléments des parcours
#'   sont dans des colonnes distinctes.
#' @param sep Une chaîne de caractères (par défaut = "-"). Le séparateur
#'   utilisé dans la colonne path.
#'
#' @return Un dataframe avec un comptage des parcours.
aggregate_by_path <- function(data, cols = NULL, path = NULL, sep = "-") {
  if (is.null(cols) && is.null(path)) {
    stop("You have to use one of the 'cols' or 'path' argument.")
  }
  if (!is.null(cols) && !is.null(path)) {
    stop("You cannot use the 'cols' and the 'path' argument at the same time")
  }

  data[["count"]] <- 1L

  if (is.null(cols)) {
    data <- split_col(data = data, path = path, count = "count", sep = sep)
    prefix_ <- "depth"
    cols <- names(data)[names(data) != "count"]
  } else {
    prefix_ <- gsub(pattern = "(.+)_[0-9]+", replacement = "\\1", cols[1])
  }

  data[is.na(data)] <- 'NA'
  data <- stats::aggregate(
    formula = stats::as.formula(paste0("count", "~",
                                       paste0(cols, collapse = "+"))),
    data = data,
    FUN = sum
  )
  data[data == 'NA'] <- NA
  names(data) <- c(paste0(prefix_, "_", seq_along(cols)), "count")
  return(data)
}

#' Crée un dataframe sous la forme interaction "source", interaction "cible",
#' comptage et niveau de profondeur associés
#'
#' Crée un dataframe sous la forme interaction "source", interaction "cible",
#' comptage et niveau de profondeur associés.
#'
#' @param data Un dataframe avec des parcours par individu. Les parcours peuvent
#'   être dans une colonne unique ou bien dans des colonnes distinctes.
#' @param cols Un vecteur de chaînes de caractères (par défaut = NULL). Les
#'   colonnes contenant les éléments des parcours individuels. Le nom de ces
#'   colonnes doit être uniformisé sous la forme <prefix>_<niveau_de_profondeur>
#'   (par exemple "depth_1", "depth_2", etc.). Utiliser la valeur NULL si les
#'   parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours individuels sont contenus dans une seule colonne
#'   qui doit être séparée. Utiliser la valeur NULL si les éléments des parcours
#'   sont dans des colonnes distinctes.
#' @param sep Une chaîne de caractères (par défaut = "-"). Le séparateur
#'   utilisé dans la colonne path.
#' @param depth Un entier (par défaut = NULL). Conserve l'ensemble des parcours
#'   d'une profondeur donnée. Si NULL, les données sont laissées comme telles.
#' @param fill Une chaîne de caractères (par défaut = " "). Si l'argument depth
#'   est utilisé, permet de spécifier la valeur remplacant les valeurs
#'   manquantes.
#'
#' @return Un dataframe contenant l'interaction "source", l'interaction "cible",
#'   le comptage et le niveau de profondeur associés.
#' @export
from_id_paths <- function(data, cols = NULL, path = NULL,
                          sep = "-", depth = NULL, fill = " ") {

  if (is.null(cols)) {
    prefix_ <- "depth"
  } else {
    prefix_ <- gsub(pattern = "(.+)_[0-9]+", replacement = "\\1", cols[1])
  }

  data <- aggregate_by_path(data, cols = cols, path = path, sep = sep)

  data <- keep_paths(data = data, depth = depth, prefix = prefix_, fill = fill)

  length_ <- ncol(data) - 2L
  data <- lapply(seq_len(length_), count_paths, data = data, prefix = prefix_)

  data <- do.call("rbind", data)
}

#' Crée les "nodes"
#'
#' Crée les "nodes".
#'
#' @param data Un dataframe sous la forme interaction "source", interaction
#'   "cible", comptage et niveau de profondeur associés.
#' @param source Une chaîne de caractères (par défaut = "source"). Le nom de la
#'   colonne contenant l'interaction source.
#' @param target Une chaîne de caractères (par défaut = "target"). Le nom de la
#'   colonne contenant l'interaction cible.
#' @param depth  Une chaîne de caractères (par défaut = "depth"). Le nom de la
#'   colonne contenant la profondeur de l'interaction.
#'
#' @return Un dataframe contenant les "nodes".
#' @export
create_nodes <- function(data, source = "source", target = "target",
                         depth = "depth") {
  data[[source]] <- paste0(data[[source]], "_", data[[depth]])
  data[[target]] <- paste0(data[[target]], "_", data[[depth]] + 1L)

  nodes <- data.frame(
    node = unique(c(data[[source]], data[[target]]))
  )
  nodes[["index"]] <- seq_len(nrow(nodes)) - 1L
  return(nodes)
}

#' Crée les "links"
#'
#' Crée les "links".
#'
#' @param data Un dataframe sous la forme interaction "source", interaction
#'   "cible", comptage et niveau de profondeur associés.
#' @param nodes Un dataframe contenant les "nodes".
#' @param source Une chaîne de caractères (par défaut = "source"). Le nom de la
#'   colonne contenant l'interaction source dans le dataframe 'data'.
#' @param target Une chaîne de caractères (par défaut = "target"). Le nom de la
#'   colonne contenant l'interaction cible dans le dataframe 'data'.
#' @param depth  Une chaîne de caractères (par défaut = "depth"). Le nom de la
#'   colonne contenant la profondeur de l'interaction dans le dataframe 'data'.
#' @param node Une chaîne de caractères (par défaut = "node"). Le nom de la
#'   colonne contenant les nodes dans le dataframe 'node'.
#' @param index Une chaîne de caractères (par défaut = "index"). Le nom de la
#'   colonne contenant les index des nodes dans le dataframe 'node'.
#'
#' @return Un dataframe contenant les "links".
#' @export
create_links <- function(data, nodes, source = "source", target = "target",
                         depth = "depth", node = "node", index = "index") {
  data[[source]] <- paste0(data[[source]], "_", data[[depth]])
  data[[target]] <- paste0(data[[target]], "_", data[[depth]] + 1L)

  links <- merge(data, nodes, by.x = source, by.y = node, all.x = TRUE)
  names(links)[names(links) == index] <- "source_index"

  links <- merge(links, nodes, by.x = target, by.y = node, all.x = TRUE)
  names(links)[names(links) == index] <- "target_index"
  return(links)
}

#' Associe des couleurs à des interactions
#'
#' Associe des couleurs à des interactions.
#'
#' @param nodes Un vecteur de chaînes de caractères. Les interactions.
#' @param colors Un vecteur de chaînes de caractères. Les couleurs.
#'
#' @return Un vecteur associant interactions et couleurs.
#' @export
choose_colors <- function(nodes, colors) {
  paste0('d3.scaleOrdinal().domain([', '"',
         paste0(nodes, collapse = '","'), '"',
         ']).range([', '"',
         paste0(colors, collapse = '","'), '"', '])')
}