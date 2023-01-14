with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
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
            -- Affiche les branches de gauche
            Afficher_Arbre(Arbre.all.Gauche);

            -- Si on tombe sur une feuille, on affiche ses donnÃ©es
            if Arbre.all.Feuille then
                Put("| ");
                Put_IP(Arbre.all.Destination); Put("     ");
                Put_IP(Arbre.all.Masque); Put("     ");
                Put(Arbre.all.Frequence,3); Put("          ");
                Put_line(To_String(Arbre.all.Port));
            else
                null;
            end if;
            
            -- Affiche les branches de droite
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

    
    procedure Comparer_Arbre (Arbre : in T_Arbre ;
                              IP : in T_Adresse_IP;
                              Destination : in out T_Adresse_IP;
                              Masque : in out T_Adresse_IP;
                              Port : in out Unbounded_String;
                              Frequence : in out Integer) is
    begin
        if Est_Vide (Arbre) then
            null;
        elsif Arbre.all.Feuille and ((IP and Arbre.all.Masque) = Arbre.all.Destination) and Masque < Arbre.all.Masque then
            Destination := Arbre.all.Destination;
            Masque := Arbre.all.Masque;
            Port := Arbre.all.Port;
            Frequence := Arbre.all.Frequence;
        else
            Comparer_Arbre(Arbre.all.Gauche, IP, Destination, Masque, Port, Frequence);
            Comparer_Arbre(Arbre.all.Droite, IP, Destination, Masque, Port, Frequence);
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
        -- Ajouter une nouvelle feuille
        if Est_Vide (Arbre) then
            Nouv_Cellule := new T_Noeud'(Destination, Masque, Port, True, Rang, Frequence, Null, Null);
            Arbre := Nouv_Cellule;
        
        -- Modifier une feuille
        elsif Arbre.all.Feuille and Destination = Arbre.all.Destination then
            Arbre.all.Masque := Masque;
            Arbre.all.Port := Port;
            Arbre.all.Rang := Rang;
            Arbre.all.Frequence := Frequence;
        
        -- Pousser une feuille à gauche ou à droite
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
    
    
    procedure Supprimer_Inutile (Arbre : in out T_Arbre) is
        Supr : T_Arbre;
    begin
        if Arbre.all.Feuille then
            null;
        elsif Est_Vide(Arbre.all.Gauche) and then Arbre.all.Droite.all.Feuille then
            Supr := Arbre;
            Arbre := Arbre.all.Droite;
            Free (Supr);
        elsif Est_Vide(Arbre.all.Droite) and then Arbre.all.Gauche.all.Feuille then
            Supr := Arbre;
            Arbre := Arbre.all.Gauche;
            Free (Supr);
        else
            null;
        end if;
    end Supprimer_Inutile;

        
    procedure Supprimer_Destination (Arbre : in out T_Arbre;
                                     Destination : in T_Adresse_IP) is
    begin
        if Est_Vide (Arbre) then
            null;
        elsif Arbre.all.Feuille and Destination = Arbre.all.Destination then
            Free (Arbre);
            Arbre := Null;
        else
            Supprimer_Destination (Arbre.all.Gauche, Destination);
            Supprimer_Destination (Arbre.all.Droite, Destination);
            Supprimer_Inutile (Arbre);
        end if;
    end Supprimer_Destination;
    
    
    procedure Vider (Arbre : in out T_Arbre) is
    begin
        if Est_Vide (Arbre) then
            null;
        else
            Vider(Arbre.all.Gauche);
            Vider(Arbre.all.Droite);
            Free (Arbre);
            Arbre := Null;
        end if;
    end Vider;
     
     
end Prefix_Tree;



