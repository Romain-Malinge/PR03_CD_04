with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with LCA_IP;                    use LCA_IP;


package Routeur_Functions is

    -- Initialiser une adresse IP
    procedure Set_IP (IP : in out LCA_IP.T_Adresse_IP;
                      N1 : in LCA_IP.T_Adresse_IP;
                      N2 : in LCA_IP.T_Adresse_IP;
                      N3 : in LCA_IP.T_Adresse_IP;
                      N4 : in LCA_IP.T_Adresse_IP);


    -- Afficher une adresse IP
    procedure Put_IP (IP : in LCA_IP.T_Adresse_IP);


    -- Afficher les parametres du programmes
    procedure Afficher_Parametres (Nom_Paquet : Unbounded_String;
                                    Nom_Table : Unbounded_String;
                                    Nom_Resultat : Unbounded_String;
                                    Taille_Cache : Integer;
                                    Politique : Unbounded_String;
                                    Nbr_Ajoute : Integer;
                                    Num_Ligne : Ada.Text_IO.Count);


    -- Afficher une cellule de Lca
    procedure Afficher_Cellule (D : in out LCA_IP.T_Adresse_IP;
                                M : in out LCA_IP.T_Adresse_IP;
                                P : in out Unbounded_String;
                                F : in out Integer);


    -- Vérifier que le motif .txt est dans un Unbounded_String
    function txt_present (Mot : in Unbounded_String) return Boolean;


    -- Consomme les carractères d'un fichier pour en faire une Adresse IP
    procedure Get_IP (Fichier : in out File_type; IP : in out T_Adresse_IP);


    -- Transforme une IP sous forme de chaine de carractère en T_Adresse_IP
    procedure To_Adresse_IP (Texte : Unbounded_String; IP : in out T_Adresse_IP);


    -- Ecrit dans un fichier le couple IP Interface
    procedure Put_IP_Interface (Fichier : File_Type;
                                IP : T_Adresse_IP;
                                Port : Unbounded_String);

    -- Supprimer les élléments en trop dans une Lca pour lui donné une taille choisie
    procedure Rogner (Lca : in out T_LCA_IP; Taille : in Integer) with
            Post =>  LCA_IP.Taille (Lca) = Taille;        -- la Lca est à la taille souhaité


end Routeur_Functions;
