-- Définition le type arbre permettant de modéliser le cache 

with LCA_IP; use LCA_IP;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Prefix_Tree is
     
     -- Définition de type
     type T_Arbre is limited private;
     
     -- Initialiser un arbre 
     procedure Initialiser (Arbre : out T_Arbre) with 
       Post => Est_Vide (Arbre); 
     
     -- Est-ce qu'un arbre est vide ? 
     function Est_Vide (Arbre : in T_Arbre) return Boolean;
     
     -- Afficher un arbre 
     procedure Afficher (Arbre : in T_Arbre);
     
     -- Obtenir le nombre d'éléments d'un arbre. 
     function Taille (Arbre : in T_Arbre) return Integer with
       Post => Taille'Result >= 0
       and (Taille'Result = 0) = Est_Vide (Arbre);
     
     -- Rafraichir le rang d'une feuille de l'arbre.
     procedure Refresh (Arbre : in out T_Arbre; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP) ;
     
     -- Chercher la feuille de plus petit rang et retourne dans le masque et l'ip de cette feuille dans les arguments out
     function Least_ranked (Arbre : in T_Arbre; Destination : out T_Adresse_IP; Masque : out T_Adresse_IP) return Boolean; 
     
     -- Supprimer une feuille donné de l'arbre
     procedure Supprimer (Arbre : in T_Arbre; Desination : in T_Adresse_IP; Masque : in T_Adresse_IP); 
     
     -- Enregistre une feuille dans l'arbre 
     procedure Enregistrer (Arbre : in T_Arbre; Destination : in T_Adresse_IP; 
                            Masque : in T_Adresse_IP;
                            Port : in Unbounded_String) with
       Post => Destination_Presente (Arbre, Destination)
       and (not (Destination_Presente (Arbre, Destination)'Old) or Taille (Arbre) = Taille (Arbre)'Old)
       and (Destination_Presente (Arbre, Destination)'Old or Taille (Arbre) = Taille (Arbre)'Old + 1);
     
     -- Savoir si une destination est présente dans une Lca.
     function Destination_Presente (Arbre : in T_Arbre ; Destination : in T_Adresse_IP) return Boolean;
     
     -- Vider un arbre 
     procedure Vider (Arbre : in T_Arbre) with 
       Post => Est_Vide (Arbre);
     
private
     
     type T_Noeud; 
     
     type T_Arbre is access T_Noeud;
     
     type T_Noeud is record 
          Destination : T_Adresse_IP;
          Masque : T_Adresse_IP; 
          Port : Unbounded_String;
          Feuille : Boolean; 
          Gauche : T_Arbre;
          Droite : T_Arbre;
          Rang : Integer;
     end record; 
   
end Prefix_Tree;
