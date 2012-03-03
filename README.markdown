Quelques programmes sur les tresses
===================================

Le code présent ici permet de transformer des mots de tresses en forme canonique (liste de
permutations) et de les manipuler (comparaison, produit, inverse, conjugaison...).
Pour tester, il y a un petit programme simulant un protocole d'échange de clé
inspiré de Diffie-Hellman employant la conjugaison par des tresses commutant entre elles.

Compilation :
    ocamlbuild DiffieHellman.byte
pour compiler en bytecode, et
    ocamlbuild DiffieHellman.native
pour compiler en natif avec un compilateur capable d'optimisations.
