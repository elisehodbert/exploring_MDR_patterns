1_data_prep (environ 3 minutes)

Importation des données brutes d'antibiogrammes, modification des jeux de données :
- suppression des colonnes contenant des antibiotiques qu'on n'étudie pas finalement
- mise en forme des colonnes
- suppression des lignes contenant des carbapénémases
- création de jeux de données séparés pour les BLSE et non-BLSE

Et sauvegarde des jeux de données modifiés dans un fichier "donnees.RData"

2_graphes_descriptifs
Prend en entrée le fichier donnees.RData
Rend en sortie, pour les bactéries BLSE et non-BLSE :
- pour chaque année et type (BLSE / non-BLSE), histogrammes de prévalence de résistance aux antibiotiques
- pour chaque année et type (BLSE / non-BLSE), histogrammes de nombre de résistances aux antibiotiques

3_simulation_bdd (environ 45 minutes)
Pour chaque jeu de données (combinaison année / BLSE ou non-BLSE), simulation de 100 jeux de données sous l'hypothèse H0 : Les résistances aux antibiotiques sont indépendantes.

4_apriori
Application de l'algorithme Apriori sur
1. les bases de données observées
2. les bases de données simulées, en utilisant du calcul en parallèle

5_filtre_itemsets
Filtre des itemsets observés à partir des mesures de qualité (eSupport et cLift)

6_describ_itemsets
Descirption des itemsets observés:
- nombre d'itemsets après filtre par minsup
- nombre d'itemsets après filtre par eSup et cLift
- distribution du nombre d'antibiotiques par pattern (?)
- distribution du nombre de familles d'antibiotiques par pattern (?)
- patterns les plus fréquents '?)

7_plot_reseau
Affichage des associations restantes sous la forme d'un plot réseau




Scripts contenant des fonctions :

asso_rule_mining_functions : description dans le fichier word associé

verif_calcul_cLift : pour vérifier que ma manière de calculer le cLift donne bien les mêmes résultats que la manière de Cazer 2021.

plot_functions : faire un fichier word pour expliquer

