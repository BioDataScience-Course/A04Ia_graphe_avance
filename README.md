# Graphiques avancés - étude de l'insuffisance rénale chronique (IRC)

## Avant-propos

Il est possible que ce document évolue au cours du temps. N'hésitez pas à aller vérifier le lien suivant afin de voir les modifications dans les consignes : https://github.com/BioDataScience-Course/A04Ia_graphe_avance

## Présentation des données

Le jeu de données utilisé dans le cadre de cet exercices a été constitué par *L. Jerlin Rubini*, avec la collaboration des docteurs *P. Soundarapandian* et *P. Eswaran*. Pour plus d'information voir https://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease.

Les données reprennent des tests sanguins et d'autres mesures de patients avec ou sans *insuffisances rénales chroniques** (IRC, ou _**C**hronic **K**idney **D**isease_ [__CKD__] en anglais). Cette information est encodée dans la variable `class`. Les données proviennent de patients suivis pendant environ deux mois avant juillet 2015, dans un hôpital du Tamil Nadu, au sud de l'Inde.

## Objectif

Ce projet est un projet **individuel**, **court** et **cadré** qui doit être **terminé pour la fin du module 4**. L'objectif est de compléter votre apprentissage dans la visualisation des données en réalisant :

- des graphiques en barre
- des boîtes de dispersion
- des figures composées

L'interprétation correcte et complète de manière synthétique des graphique est également un aspect important de ce travail. Attention : ne décrivez pas "techniquement" vos graphiques comme par exemple "graphique en barres verticales de Y sur X utilisant la couleur.". Ce genre de phrase est inutile. Le lecteur le voit bien et il est censé savoir de quel type de graphique il s'agit. Vous devez par contre expliciter les points importants qui ressortent de l'*analyse* du graphique. Par exemple "les patients atteints d'IRC présentent une valeur plus élemvée de Y, avec même trois valeurs extrêmes. La dispesion des données (espace interquartile) est également plus grande, et elle est par ailleurs asymétrique. Les patients sains par contre, ..."

## Consignes 

Au sein du fichier `insuffisance_renale_chronique.Rmd` qui se trouve dans le dossier `docs`, réalisez et interprétez les différents graphiques demandés. Une fois le travail réalisé, **assurez-vous que le document compile en un rapport final HTML sans erreurs via le bouton `Knit`**, sinon, corrigez les erreurs qui s'affichent avant votre soumission finale.
