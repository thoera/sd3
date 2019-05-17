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
#'   colonnes contenant les éléments des parcours agrégés. L'ordre est
#'   important, il détermine le sens des éléments. Utiliser la valeur NULL si
#'   les éléments des parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours agrégés sont contenus dans une seule colonne qui
#'   doit être séparée. Utiliser la valeur NULL si les éléments des parcours
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
#'
#' @examples
#' # parcours dans une colonne unique
#' from_aggregated_paths(data = data_1, path = "parcours",
#'                       count = "comptage", sep = " - ")
#'
#' # parcours dans plusieurs colonnes
#' cols <- paste0("contact_", 1:5)
#' from_aggregated_paths(data = data_2, cols = cols, count = "comptage")
#'
#' @export
from_aggregated_paths <- function(data, cols = NULL, path = NULL,
                                  count = "count", sep = "-",
                                  depth = NULL, fill = " ") {
  if (is.null(cols) && is.null(path)) {
    stop("You have to use one of the 'cols' or 'path' argument.")
  }
  if (!is.null(cols) && !is.null(path)) {
    stop("You cannot use the 'cols' and the 'path' argument at the same time.")
  }

  if (is.null(cols)) {
    data <- split_col(data = data, path = path, count = count, sep = sep)
  } else {
    data <- data[, c(cols, count)]
    names(data) <- c(paste0("depth_", seq_along(cols)), "count")
  }

  count_ <- "count"
  prefix_ <- "depth"

  data <- keep_paths(data = data, depth = depth, prefix = prefix_, fill = fill)

  length_ <- ncol(data) - 2L
  data <- lapply(seq_len(length_), count_paths, data = data,
                 count = count_, prefix = prefix_)

  data <- do.call("rbind", data)
  return(data)
}

#' Agrège des parcours individuels
#'
#' Agrège des parcours individuels.
#'
#' @param data Un dataframe avec des parcours par individu.
#' @param cols Un vecteur de chaînes de caractères (par défaut = NULL). Les
#'   colonnes contenant les éléments des parcours individuels. L'ordre est
#'   important, il détermine le sens des éléments du parcours. Utiliser la
#'   valeur NULL si les éléments des parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours individuels sont contenus dans une seule colonne
#'   qui doit être séparée. Utiliser la valeur NULL si les éléments des parcours
#'   sont dans des colonnes distinctes.
#' @param sep Une chaîne de caractères (par défaut = "-"). Le séparateur utilisé
#'   dans la colonne path.
#'
#' @return Un dataframe avec un comptage des parcours.
#'
#' @examples
#' # parcours dans une colonne unique
#' aggregate_by_path(data = data_3, path = "parcours", sep = "-")
#'
#' # parcours dans plusieurs colonnes
#' cols <- paste0("contact_", 1:5)
#' aggregate_by_path(data = data_4, cols = cols)
#'
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @export
aggregate_by_path <- function(data, cols = NULL, path = NULL, sep = "-") {
  if (is.null(cols) && is.null(path)) {
    stop("You have to use one of the 'cols' or 'path' argument.")
  }
  if (!is.null(cols) && !is.null(path)) {
    stop("You cannot use the 'cols' and the 'path' argument at the same time.")
  }
  data_ <- data.table::copy(data)
  data.table::setDT(data_)
  data_[, ("count") := 1L]

  if (is.null(cols)) {
    data_ <- split_col(data = data_, path = path, count = "count", sep = sep)
    data.table::setDT(data_)
  } else {
    data_ <- data_[, .SD, .SDcols = c(cols, "count")]
    data.table::setnames(data_, c(paste0("depth_", seq_along(cols)), "count"))
  }

  cols_ <- setdiff(names(data_), "count")
  data_ <- data_[, list("count" = sum(get("count"))), by = cols_]
  data.table::setnames(data_, c(paste0("depth_", seq_along(cols_)), "count"))
  return(data.table::setDF(data_))
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
#'   colonnes contenant les éléments des parcours individuels. L'ordre est
#'   important, il détermine le sens des éléments du parcours. Utiliser la
#'   valeur NULL si les éléments des parcours sont dans une colonne unique.
#' @param path Une chaîne de caractères (par défaut = NULL). Le nom de la
#'   colonne si les parcours individuels sont contenus dans une seule colonne
#'   qui doit être séparée. Utiliser la valeur NULL si les éléments des parcours
#'   sont dans des colonnes distinctes.
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
#'
#' @examples
#' # parcours dans une colonne unique
#' from_id_paths(data = data_3, path = "parcours", sep = "-")
#'
#' # parcours dans plusieurs colonnes
#' cols <- paste0("contact_", 1:5)
#' from_id_paths(data = data_4, cols = cols)
#'
#' @export
from_id_paths <- function(data, cols = NULL, path = NULL,
                          sep = "-", depth = NULL, fill = " ") {
  data <- aggregate_by_path(data, cols = cols, path = path, sep = sep)

  data <- keep_paths(data = data, depth = depth, fill = fill)

  length_ <- ncol(data) - 2L
  data <- lapply(seq_len(length_), count_paths, data = data)

  data <- do.call("rbind", data)
  return(data)
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
#'
#' @examples
#' cols <- paste0("contact_", 1:5)
#' paths <- from_id_paths(data = data_4, cols = cols)
#'
#' create_nodes(data = paths)
#'
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
#'
#' @examples
#' cols <- paste0("contact_", 1:5)
#' paths <- from_id_paths(data = data_4, cols = cols)
#'
#' nodes <- create_nodes(data = paths)
#' create_links(data = paths, nodes = nodes)
#'
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
#'
#' @examples
#' choose_colors(nodes = c("A", "B"), colors = c("#50BE87", "#4BB4E6"))
#'
#' @export
choose_colors <- function(nodes, colors) {
  paste0('d3.scaleOrdinal().domain([', '"',
         paste0(nodes, collapse = '","'), '"',
         ']).range([', '"',
         paste0(colors, collapse = '","'), '"', '])')
}
