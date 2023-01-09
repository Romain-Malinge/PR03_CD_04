with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Routeur_Exceptions;          use Routeur_Exceptions;
with Ada.Unchecked_Deallocation;
with Routeur_Functions;           use Routeur_Functions;

package body Prefix_Tree is

    procedure Free is new Ada.Unchecked_Deallocation (Object => T_Noeud,
                                                      Name => T_Arbre);


    procedure Initialiser (Arbre : out T_Arbre) is
    begin
        Arbre := Null;
    end Initialiser;


    function Est_Vide (Arbre : T_Arbre) return Boolean is
    begin
        return Arbre = Null;
    end Est_Vide;


    procedure Afficher_Arbre (Arbre : in T_Arbre) is
    begin
        if Est_Vide(Arbre) then
            null;
        else
            -- On affiche de gauche Ã  droite
            Afficher_Arbre(Arbre.all.Gauche);

            -- Si on tombe sur une feuille, on affiche ses donnÃ©es
            if Arbre.all.Feuille then
                Put("| ");
                Put_IP(Arbre.all.Destination); Put("     ");
                Put_IP(Arbre.all.Masque); Put("     ");
                Put(To_String(Arbre.all.Port)); Put("        ");
                Put_Line(Arbre.all.Frequence'Image);
            else
                null;
            end if;
            Afficher_Arbre(Arbre.all.Droite);
        end if;
    end Afficher_Arbre;


    function Taille (Arbre : in T_Arbre) return Integer is
    begin
        if Est_Vide(Arbre) then
            return 0;
        elsif Arbre.all.Feuille then
            return 1;
        else
            return (Taille(Arbre.all.Gauche) + Taille(Arbre.all.Droite));
        end if;
    end Taille;

    
    -- Sous-programme utile pour déterminer le rang maximum dans l'arbre
    function Rang_Max (Arbre : in T_Arbre) return Integer is
    begin
        if Est_Vide(Arbre) then
            return 0;
        elsif Arbre.all.Feuille then
            return Arbre.all.Rang;
        elsif Rang_Max(Arbre.all.Droite) > Rang_Max(Arbre.all.Gauche) then
            return Rang_Max(Arbre.all.Droite);
        else
            return Rang_Max(Arbre.all.Gauche);
        end if;
    end Rang_Max;

    
    procedure Comparer_Arbre (Arbre : in T_Arbre ;
                              IP : in T_Adresse_IP;
                              Destination : in out T_Adresse_IP;
                              Masque : in out T_Adresse_IP;
                              Port : in out Unbounded_String;
                              Rang : in out Integer;
                              Frequence : in out Integer;
                              Avancement : in out Integer) is
    begin
        if Est_Vide (Arbre) then
            null;
        elsif Arbre.all.Feuille and ((IP and Arbre.all.Masque) = Arbre.all.Destination) then
            Destination := Arbre.all.Destination;
            Masque := Arbre.all.Masque;
            Port := Arbre.all.Port;
            Rang := Arbre.all.Rang;
            Frequence := Arbre.all.Frequence;
        elsif not(Arbre.all.Feuille) and ((IP and 2**(31-Avancement)) = 0) then
            Avancement := Avancement + 1;
            Comparer_Arbre(Arbre.all.Gauche, IP, Destination, Masque, Port, Rang, Frequence, Avancement);
        else
            Avancement := Avancement + 1;
            Comparer_Arbre(Arbre.all.Droite, IP, Destination, Masque, Port, Rang, Frequence, Avancement);
        end if;
    end Comparer_Arbre;

    
    procedure Enregistrer (Arbre : in out T_Arbre;
                           Destination : in T_Adresse_IP; 
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String; 
                           Rang : in Integer;
                           Frequence : in Integer;
                           Avancement : in out Integer) is
        Nouv_Cellule : T_Arbre;
    begin
        -- ajouter une nouvelle feuille
        if Est_Vide (Arbre) then
            Nouv_Cellule := new T_Noeud'(Destination, Masque, Port, True, Rang, Frequence, Null, Null);
            Arbre := Nouv_Cellule;
        
            -- modifier une feuille
        elsif Arbre.all.Feuille and Destination = Arbre.all.Destination then
            Arbre.all.Masque := Masque;
            Arbre.all.Port := Port;
            Arbre.all.Rang := Rang;
            Arbre.all.Frequence := Frequence;
        
            -- pousser une feuille
        elsif Arbre.all.Feuille and not(Destination = Arbre.all.Destination) then
            Nouv_Cellule := new T_Noeud;
            if ((Arbre.all.Destination and 2**(31-Avancement)) = 0) then
                Nouv_Cellule.all.Gauche := Arbre;
                Nouv_Cellule.all.Droite := Null;
            else
                Nouv_Cellule.all.Gauche := Null;
                Nouv_Cellule.all.Droite := Arbre;
            end if;
            Nouv_Cellule.all.Feuille := False;
            Arbre := Nouv_Cellule;
            Enregistrer (Arbre, Destination, Masque, Port, Rang, Frequence, Avancement);

        elsif not(Arbre.all.Feuille) and ((Destination and 2**(31-Avancement)) = 0) then
            Avancement := Avancement + 1;
            Enregistrer (Arbre.all.Gauche, Destination, Masque, Port, Rang, Frequence, Avancement);
        else
            Avancement := Avancement + 1;
            Enregistrer (Arbre.all.Droite, Destination, Masque, Port, Rang, Frequence, Avancement);
        end if ;
    end Enregistrer;

    
    procedure Least_ranked (Arbre : in T_Arbre;
                            Destination : in out T_Adresse_IP;
                            Min : in out Integer) is
    begin
        if Est_Vide (Arbre) then
            null;
        elsif Arbre.all.Feuille and Arbre.all.Rang < Min then
            Destination := Arbre.all.Destination;
            Min := Arbre.all.Rang;
        else
            Least_ranked(Arbre.all.Gauche, Destination, Min);
            Least_ranked(Arbre.all.Droite, Destination, Min);
        end if;
    end Least_ranked;

    
    procedure Supprimer_Rang_Min (Arbre : in T_Arbre;
                                  Desination : in T_Adresse_IP) is
        Supr : T_Arbre;
    begin
        if Est_Vide (Arbre) then
            null;
        elsif Desination = Arbre.all.Destination then
            Supr := Arbre;
            Free (Supr);
        elsif not Est_Vide(Arbre.all.Gauche) and not Est_Vide(Arbre.all.Droite) then
        else
    end Supprimer_Rang_Min;
    
    
    procedure Vider (Arbre : in T_Arbre) is
    begin
        null;
    end Vider;
     
     
end Prefix_Tree;
