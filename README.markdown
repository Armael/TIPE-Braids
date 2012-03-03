Quelques programmes sur les tresses
===================================

Le code présent ici permet de transformer des mots de tresses en forme canonique (liste de
permutations) et de les manipuler (comparaison, produit, inverse, conjugaison...).
Pour tester, il y a un petit programme simulant un protocole d'échange de clé
inspiré de Diffie-Hellman employant la conjugaison par des tresses commutant entre elles.

Compilation : ocamlbuild DiffieHellman.byte


Il est possible d'exporter le code source grâce à ocamlweb. Pour ce faire, pour convertir
en latex le code du fichier Canonical.ml par exemple :

    ocamlweb Canonical.ml -o Canonical.tex
    sed -i 's/latin1/utf8/' Canonical.tex # Workaround au sujet de l'encodage en attendant
                                          # une meilleure solution
