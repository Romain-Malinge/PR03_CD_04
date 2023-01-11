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
    procedure Afficher_Arbre (Arbre : in T_Arbre);
     
    -- Obtenir le nombre d'éléments d'un arbre. 
    function Taille (Arbre : in T_Arbre) return Integer with
            Post => Taille'Result >= 0
            and (Taille'Result = 0) = Est_Vide (Arbre);
    

     
    -- Router une IP en la comparent à toutes les feuilles d'un arbre
    procedure Comparer_Arbre (Arbre : in T_Arbre ;
                              IP : in T_Adresse_IP;
                              Destination : in out T_Adresse_IP;
                              Masque : in out T_Adresse_IP;
                              Port : in out Unbounded_String;
                              Frequence : in out Integer;
                              Avancement : in out Integer);

    -- Enregistre une feuille dans l'arbre 
    procedure Enregistrer (Arbre : in out T_Arbre;
                           Destination : in T_Adresse_IP; 
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String; 
                           Rang : in Integer;
                           Frequence : in Integer;
                           Avancement : in out Integer);
   
    
    -- Chercher les informations de la feuille avec le plus petit rang
    procedure Least_ranked (Arbre : in T_Arbre;
                            Destination : in out T_Adresse_IP;
                            Min : in out Integer); 
    
    -- Supprimer une feuille donnée de l'arbre
    procedure Supprimer_Rang_Min (Arbre : in out T_Arbre;
                                  Destination : in T_Adresse_IP);
     
    -- Vider un arbre 
    procedure Vider (Arbre : in out T_Arbre) with 
            Post => Est_Vide (Arbre);
     
private
     
    type T_Noeud; 
     
    type T_Arbre is access T_Noeud;
     
    type T_Noeud is record 
        Destination : T_Adresse_IP;
        Masque : T_Adresse_IP; 
        Port : Unbounded_String;
        Feuille : Boolean; 
        Rang : Integer;
        Frequence : Integer;
        Gauche : T_Arbre;
        Droite : T_Arbre;
    end record; 
   
end Prefix_Tree;
