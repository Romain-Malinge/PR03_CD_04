-- DÃ©finition d'une lca pour l'engeristrement de
-- quadruplet Destination, Masque, Port, Frequence

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package LCA_IP is
    
    -- DÃ©finition des types
    type T_LCA_IP is limited private;
    type T_Adresse_IP is mod 2**32;
    
    
    -- Initialiser une Lca. La Lca est vide
    procedure Initialiser (Lca: out T_LCA_IP) with
            Post => Est_Vide (Lca);
    
    
    -- Est-ce qu'une Lca est vide ?
    function Est_Vide (Lca : in T_LCA_IP) return Boolean;
    
    
    -- Savoir si une destination est prÃ©sente dans une Lca.
    function Destination_Presente (Lca : in T_LCA_IP ; Destination : in T_Adresse_IP) return Boolean;
    

    -- Obtenir le nombre d'Ã©lÃ©ments d'une Lca
    function Taille (Lca : in T_LCA_IP) return Integer with
            Post => Taille'Result >= 0
            and (Taille'Result = 0) = Est_Vide (Lca);
    
    
    -- Obtenir la premiÃ¨re frÃ©quence d'une Lca
    function La_Frequence_Premier (Lca : in out T_LCA_IP) return Integer;
    
    
    -- Actualise le masque le plus petit qui permet de ne pas faire d'erreur de cache
    procedure Trouver_Grand_Masque (Lca : in out T_LCA_IP;
                                    Destination : in T_Adresse_IP;
                                    IP : in T_Adresse_IP;
                                    Grand_Masque : in out T_Adresse_IP);
    

    -- Enregistrer par le haut un quadruplet Destination, Masque, Port, Frequence dans une Lca
    -- Si la destination est dÃ©ja prÃ©sente: modifie les donnÃ©es de la cellules
    procedure Enregistrer (Lca : in out T_LCA_IP;
                           Destination : in T_Adresse_IP;
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String;
                           Frequence : in Integer :=0) with
            Post => Destination_Presente (Lca, Destination)
            and (not (Destination_Presente (Lca, Destination)'Old) or Taille (Lca) = Taille (Lca)'Old)
            and (Destination_Presente (Lca, Destination)'Old or Taille (Lca) = Taille (Lca)'Old + 1);
    
    
    -- Supprimer la cellule associÃ©e Ã  une Destination et un Masque dans une Lca.
    -- Exception : Destination_Absente_Exception si Destination n'est pas prÃ©sente dans la Lca
    procedure Supprimer (Lca : in out T_LCA_IP ;
                         Destination : in T_Adresse_IP) with
            Post =>  Taille (Lca) = Taille (Lca)'Old - 1       -- un Ã©lÃ©ment de moins
            and not Destination_Presente (Lca, Destination);   -- la destination a Ã©tÃ© suprimÃ©
    
    
    -- Supprimer la derniÃ¨re cellule dans une Lca.
    procedure Supprimer_Premier (Lca : in out T_LCA_IP) with
            Post =>  Taille (Lca) <= Taille (Lca)'Old;         -- un Ã©lÃ©ment de moins
    
    
    -- Supprimme la destination la moins fréquement utilisé d'une Lca qui n'est pas le dernier ajout au cache
    procedure Supprimer_LFU (Lca : in out T_LCA_IP; Dernier_Ajout : in T_Adresse_IP) with
            Post =>  Taille (Lca) <= Taille (Lca)'Old          -- un Ã©lÃ©ment de moins
            and Destination_Presente (Lca, Dernier_Ajout);     -- le dernier ajout au cache est toujour présent


    -- Supprimer tous les éléments d'une Lca.
    procedure Vider (Lca : in out T_LCA_IP) with
            Post => Est_Vide (Lca);


    -- Appliquer un traitement (Traiter) pour chaque quadruplet d'une Lca.
    generic with procedure Traiter (Destination : in out T_Adresse_IP;
                                    Masque : in out T_Adresse_IP;
                                    Port : in out Unbounded_String;
                                    Frequence : in out Integer);
    
        
    -- Applique la la fonction traiter à toutes les cellues d'une Lca
    procedure Pour_Chaque (Lca : in T_LCA_IP);


private
           
    type T_Cellule;

    type T_LCA_IP is access T_Cellule;

    type T_Cellule is record
        Destination : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Port : Unbounded_String;               
        Frequence : Integer;
        Suivant : T_LCA_IP;
    end record;
    
end LCA_IP;
