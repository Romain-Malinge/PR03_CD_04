-- Définir le type arbre permettant de modéliser le cache 

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
     
     -- Savoir si une destination est prÃ©sente dans un arbre et renvoyer le port dans la variable Port
     function Comparer_Arbre (Arbre : in T_Arbre ;
                              IP : in T_Adresse_IP;
                              Masque : in T_Adresse_IP;
                              Port :  in out Unbounded_String;
                              Avancement : in Integer) return Boolean;
     
   
     -- Chercher la feuille de plus petit rang et retourne dans le masque et l'ip de cette feuille dans les arguments out
     function Least_ranked (Arbre : in T_Arbre; Destination : out T_Adresse_IP; Masque : out T_Adresse_IP) return Boolean; 
     
     -- Supprimer une feuille donnÃ© de l'arbre
     procedure Supprimer (Arbre : in T_Arbre; Desination : in T_Adresse_IP; Masque : in T_Adresse_IP); 
     
     -- Enregistre une feuille dans l'arbre 
     procedure Enregistrer (Arbre : in out T_Arbre; Destination : in T_Adresse_IP; 
                            Masque : in T_Adresse_IP;
                            Port : in Unbounded_String; 
                            Rang : in Integer;
                           Avancement : in Integer);
     
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
