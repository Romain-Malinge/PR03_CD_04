with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package LCA_IP is

    type T_LCA_IP is limited private;
    
    type T_Adresse_IP is mod 2**32;

    
    -- Initialiser une Lca. La Lca est vide.
    procedure Initialiser (Lca: out T_LCA_IP) with
      Post => Est_Vide (Lca);


    -- Est-ce qu'une Lca est vide ?
    function Est_Vide (Lca : in T_LCA_IP) return Boolean;


    -- Obtenir le nombre d'�l�ments d'une Lca. 
    function Taille (Lca : in T_LCA_IP) return Integer with
      Post => Taille'Result >= 0
      and (Taille'Result = 0) = Est_Vide (Lca);


    -- Enregistrer un quadruplet Destination,Masque,Port,Frequence une Lca d'IP.
    procedure Enregistrer (Lca : in out T_LCA_IP;
                           Destination : in T_Adresse_IP;
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String;
                           Frequence : in Integer :=0)with
      Post => Destination_Presente (Lca, Destination)
      and (not (Destination_Presente (Lca, Destination)'Old) or Taille (Lca) = Taille (Lca)'Old)
      and (Destination_Presente (Lca, Destination)'Old or Taille (Lca) = Taille (Lca)'Old + 1);

    
    -- Supprimer la cellule associ�e � une Destination dans une Lca.
    -- Exception : Destination_Absente_Exception si Destination n'est pas pr�sente dans la Lca
    procedure Supprimer (Lca : in out T_LCA_IP ; Destination : in T_Adresse_IP) with
      Post =>  Taille (Lca) = Taille (Lca)'Old - 1       -- un �l�ment de moins
      and not Destination_Presente (Lca, Destination);   -- la destination a �t� suprim�


    -- Savoir si une destination est pr�sente dans une Lca.
    function Destination_Presente (Lca : in T_LCA_IP ; Destination : in T_Adresse_IP) return Boolean;


    -- Supprimer tous les ElEments d'une Lca.
    procedure Vider (Lca : in out T_LCA_IP) with
      Post => Est_Vide (Lca);


    -- Appliquer un traitement (Traiter) pour chaque quadruplet d'une Lca.
    generic
        with procedure Traiter (Destination : in T_Adresse_IP;
                                Masque : in T_Adresse_IP;
                                Port : in Unbounded_String;
                                Frequence : in Integer);
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
