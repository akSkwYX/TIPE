# Titre

Corection deux frases incorecte en phrases correctes

## Positionnement thématique :

Informatique (Informatique Théorique)

## Motivation

Lors de ma deuxième année de classe préparatoire, j’ai découvert la théorie des langages, un domaine qui m’a immédiatement passionné. J’ai alors voulu comprendre comment les correcteurs orthographiques et grammaticaux transforment des phrases en français incorrectes en phrases correctes, ce qui m’a conduit à choisir ce sujet pour mon TIPE.

## Ancrage

L'objectif de ce TIPE est de transformer des phrases incorrectes en phrases correctes. Pour ce faire on utilise l'analyse syntaxique et lexicale dont le but est de convertir l'entrée dans un format qui rendra les données exploitables.

## Mots-clés :

- Français / French
- Grammaire / Grammar
- Analyse syntaxique / Parsing
- Analyse lexicale / Lexing
- Correction syntaxique / Syntactic correction
- Arbre syntaxique / Syntax tree
- Structure de données / Data structure

## Bibliograhie commentée :

La correction orthographique et syntaxique de phrases repose sur la théorie des langages formels. Elle trouve également des applications pratiques dans le traitement automatique des langues (TAL). Ce projet s’inscrit dans ce domaine en adoptant une approche basée sur des grammaires formelles pour traiter un modèle réduit de français.
Le point de départ est la modélisation des structures syntaxiques à l'aide des grammaires non-contextuels, une classe de grammaires formelles étudiée en détail dans des ouvrages de référence comme celui de Hopcroft et Ullman [1]. Ces grammaires permettent de définir un ensemble de règles précises pour décrire les phrases simples.

Ces règles sont ensuite utilisées par l'analyseur syntaxique pour transformer la phrase en un arbre syntaxique, permettant de vérifier la correction de la phrase et facilitant sa correction.

Cependant, avant de définire les règles qui régissent les interractions entres les mots il faut savoir qu'elles sont les mots qui peuvent être utilisé. Pour cela il faut donc stocker le dictionnair. Ainsi, le projet s'appuie sur une structure de données appelée Trie, particulièrement efficace pour vérifier rapidement si un mot appartient à un dictionnaire donné[2]. Cette structure optimise les recherches lexicales, ce qui est crucial pour traiter des phrases de manière efficace.

Dans un contexte pratique, l'utilisation de Grammalecte, un correcteur grammatical open source, met en lumière les problématiques liées à la correction grammaticale, l'analyse de la sémantique et la cohérence entre les attentes de l'utilisateur et les suggestions proposées [3]. En effet, la sémantique ne peut être entièrement capturée par une simple grammaire. Par exemple, la phrase « Le social chat entier éradique » présente une incohérence sémantique. Pour pallier ce manque, des méthodes telles que l'analyse par graphes de dépendance syntaxique ou l'utilisation de modèles de langage préentraînés peuvent être envisagées, bien que ces approches ne soient pas traitées dans ce TIPE.

Le principal enjeu étudié réside dans la cohérence entre ce qu'attend l'utilisateur et les suggestions offertes par le programme. Étant donné que la sémantique échappe à une analyse purement grammaticale, il est nécessaire de recourir à d'autres méthodes pour maximiser cette cohérence. Par exemple, la distance de Levenshtein [4] permet de mesurer la similarité entre deux mots afin de privilégier des corrections basées sur la proximité lexicale lors de la correction de fautes de frappe. Par ailleurs, l'utilisation d'un dictionnaire personnalisé à l'utilisateur permet d'orienter les propositions en fonction des habitudes linguistiques spécifiques de celui-ci.


## Problématique retenue :

Comment peut-on écrire un programme qui corrige des phrases en français ?

## Objectifs du TIPE :

L'objectif principal de ce TIPE est de développer un programme qui :

- Analyse des phrases simples en générant leur arbre syntaxique lorsque celles-ci respectent une grammaire définie.
- Corriger les erreurs syntaxiques et grammaticales en proposant une ou plusieurs corrections possibles.
- Améliorer les propositions de correction du modèle en se basant sur des données d'utilisateur pour qu'elles aient plus de chance de correspondre à ce qu'il voulait dire.

## Liste de références bibliographiques :

 [1] Hopcroft, Ullman - _Introduction to Automata Theory, Languages, and Computation_

 [2] Edward Fredkin - Trie Memory - Communications of the ACM, Volume 3, Issue 9, Pages 490 - 499 : https://dl.acm.org/doi/10.1145/367390.367400

 [3] Grammalecte - Outil open source pour la correction grammaticale en français : https://github.com/Pofilo/grammalecte/tree/master

 [4] Levenshtein, V.I. (1966) : https://nymity.ch/sybilhunting/pdf/Levenshtein1966a.pdf
