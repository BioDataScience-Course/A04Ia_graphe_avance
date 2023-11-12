# Vérifications de ckd_notebook.qmd
ckd <- parse_rmd("../../ckd_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("ckd_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("ckd_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Analyses", "Discussion et conclusion", "Références")
    %in% (rmd_node_sections(ckd) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "bar1", "bar1comment", "bar2",
    "bar2comment", "bar3a", "bar3b", "bar3comment", "bar4", "bar4comment",
    "boxplot1", "boxplot1comment", "boxplot2", "boxplot2comment", "composite",
    "compositecomment")
    %in% rmd_node_label(ckd)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(ckd))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(ckd[[1]]$author != "___")
  expect_true(!grepl("__", ckd[[1]]$author))
  expect_true(grepl("^[^_]....+", ckd[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", ckd[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", ckd[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunk 'import' : importation des données", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `ckd` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # avez modifié le code du chunk 'import' (il ne faut rien y changer).

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `ckd` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `ckd` est incorrect
  # Vérifiez l'importation des données dans le chunk d'importation `import` et
  # réexécutez-le pour corriger le problème.
})

test_that("Chunks 'bar1' & 'bar1comment' : graphique en barre des patients
  touchés  par une IRC et une coronaropathie", {
  expect_true(is_identical_to_ref("bar1"))
  # Le graphique en barres produit par 'bar1' n'est pas celui attendu
  # Avez-vous bien employé des aplats de couleurs ? Lisez bien la consigne et
  # corrigez l'erreur, puis refaites un 'Rendu' du document avant de retester.

  expect_true(is_identical_to_ref("bar1comment"))
  # L'interprétation du graphique en barres est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'bar2' & 'bar2comment' : graphique en barre des patients
  touchés par une IRC et présentant des amas de pyocytes", {
    expect_true(is_identical_to_ref("bar2"))
    # Le graphique en barres produit par 'bar2' n'est pas celui attendu
    # Avez-vous bien employé modifié l'argument 'position =' ? Lisez bien
    # la consigne et corrigez l'erreur, puis refaites un 'Rendu' du document
    # avant de retester.

    expect_true(is_identical_to_ref("bar2comment"))
    # L'interprétation du graphique en barres est (partiellement) fausse
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez
    # plus cette aide plus tard dans le travail de groupe ou les
    # interrogations !
  })

test_that("Chunks 'bar3a', 'bar3b' & 'bar3comment' : graphique en barre des
  patients touchés par une IRC et par le diabète", {
    expect_true(is_identical_to_ref("bar3a"))
    # Le graphique en barres produit par 'bar3a' n'est pas celui attendu
    # Avez-vous bien employé modifié l'argument 'position =' ? Lisez bien la
    # consigne et corrigez l'erreur, puis refaites un 'Rendu' du document avant
    # de retester.

    expect_true(is_identical_to_ref("bar3b"))
    # Le graphique en barres produit par 'bar3a' n'est pas celui attendu
    # Avez-vous bien employé modifié l'argument 'position =' ? Lisez bien la
    # consigne et corrigez l'erreur, puis refaites un 'Rendu' du document avant
    # de retester.

    expect_true(is_identical_to_ref("bar3comment"))
    # L'interprétation des deux graphiques en barres est (partiellement) fausse
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Assurez-vous de bien comprendre la différence entre les effectifs et les
    # fractions sur l'axe Y, cela change grandement l'interprétation du
    # graphique
    # Assurez-vous également de bien comprendre ce qui est coché ou pas :
    # vous n'aurez plus cette aide plus tard dans le travail de groupe ou les
    # interrogations !
  })

test_that("Chunks 'bar4' & 'bar4comment' : graphique en barre de la moyenne de
  l'urée sanguines", {
    expect_true(is_identical_to_ref("bar4"))
    # Le graphique en barres produit par 'bar4' n'est pas celui attendu
    # Avez-vous bien calculé la moyenne par groupe ?  Lisez bien la consigne et
    # corrigez l'erreur, puis refaites un 'Rendu' du document avant de retester.

    expect_true(is_identical_to_ref("bar4comment"))
    # L'interprétation du graphique en barres est (partiellement) fausse
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez
    # plus cette aide plus tard dans le travail de groupe ou les
    # interrogations !
  })

test_that("Chunks 'boxplot1' & 'boxplot1comment' : Boites de dispersion de
  l'urée sanguines", {
    expect_true(is_identical_to_ref("boxplot1"))
    # Les boites de dispersion produites par 'boxplot1' ne sont pas celles
    # attendues.  Lisez bien la consigne et corrigez l'erreur, puis refaites un
    # 'Rendu' du document avant de retester.

    expect_true(is_identical_to_ref("boxplot1comment"))
    # L'interprétation des boites de dispersion sont (partiellement) fausses
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Assurez-vous de bien comprendre les cinq nombres qui sont à la base de ce
    # graphique. Assurez-vous également de bien comprendrece qui est coché
    # ou pas : vous n'aurez plus cette aide plus tard dans le travail de groupe
    # ou les interrogations !
  })

test_that("Chunks 'boxplot2' & 'boxplot2comment' : Boites de dispersion de
  la concentration en sodium", {
    expect_true(is_identical_to_ref("boxplot2"))
    # Les boites de dispersion produites par 'boxplot2' ne sont pas celles
    # attendues. Avez vous bien ajouté les nombres d'observations au dessus de
    # chaque boite. Lisez bien la consigne et corrigez l'erreur, puis refaites
    # un 'Rendu' du document avant de retester.

    expect_true(is_identical_to_ref("boxplot2comment"))
    # L'interprétation des boites de dispersion sont (partiellement) fausses
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Assurez-vous également de bien comprendre ce qui est coché  ou pas : vous
    # n'aurez plus cette aide plus tard dans le travail de groupe ou les
    # interrogations !
  })

test_that("Chunks 'composite' & 'compositecomment' : Deux graphiques", {
    expect_true(is_identical_to_ref("composite"))
    # Le graphique composé de deux graphiques en boites de dispersion n'est pas
    # celui attendue.  Lisez bien la consigne et corrigez l'erreur, puis
    # refaites un 'Rendu' du document avant de retester.

    expect_true(is_identical_to_ref("compositecomment"))
    # L'interprétation des boites de dispersion sont (partiellement) fausses
    # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre
    # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
    # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
    # Avez vous tenu compte de la différence d'unité entre les deux graphiques.
    # Assurez-vous également de bien comprendre ce qui est coché  ou pas : vous
    # n'aurez plus cette aide plus tard dans le travail de groupe ou les
    # interrogations !
  })
