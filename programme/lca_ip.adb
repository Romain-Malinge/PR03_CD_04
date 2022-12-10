with Ada.Text_IO;            use Ada.Text_IO;
with Sda_Exceptions;         use Sda_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA_IP is

    procedure Free is new Ada.Unchecked_Deallocation (Object => T_Cellule,
                                                      Name => T_LCA_IP);


    procedure Initialiser(Lca: out T_LCA_IP) is
    begin
        Lca := null;
    end Initialiser;


    function Est_Vide (Lca : T_LCA_IP) return Boolean is
    begin
        return Lca = null;
    end;


    function Taille (Lca : in T_LCA_IP) return Integer is
    begin
        if Est_Vide(Lca) then
            return 0;
        else
            null;
        end if;
        -- Passer à la cellule suivante.
        return Taille (Lca.all.Suivant) + 1;
    end Taille;


    procedure Enregistrer (Lca : in out T_LCA_IP;
                           Destination : in T_Adresse_IP;
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String;
                           Frequence : in Integer :=0)
    is
        New_Cel : T_LCA_IP;
    begin
        -- Creer une nouvelle cellule.
        if Est_Vide(Lca) then
            New_Cel := new T_Cellule'(Destination, Masque, Port, Frequence, Lca);
            Lca := New_Cel;
        -- Modifier la donnee.
        elsif Lca.all.Destination = Destination then
            Lca.all.Masque := Masque;
            Lca.all.Port := Port;
            Lca.all.Frequence := Frequence;
        -- Passer à la cellule suivante.
        else
            Enregistrer (Lca.all.Suivant, Destination, Masque, Port, Frequence);
        end if;
    end Enregistrer;


    function Destination_Presente (Lca : in T_LCA_IP ; Destination : in T_Adresse_IP) return Boolean is
    begin
        if Est_Vide(Lca) then
            return False;
        elsif Lca.all.Destination = Destination then
            return True;
        -- Passer à la cellule suivante.
        else
            return Destination_Presente (Lca.all.Suivant, Destination);
        end if;
    end;


    procedure Supprimer (Lca : in out T_LCA_IP ; Destination : in T_Adresse_IP) is
        A_Liberer : T_LCA_IP;
    begin
        -- Lever l'exeption.
        if Est_Vide(Lca) then
            raise Destination_Absente_Exception;
        -- Supprimer sans laisser de miettes
        elsif Lca.all.Destination = Destination then
            A_Liberer := Lca;
            Lca := Lca.all.Suivant;
            Free (A_Liberer);
        -- Passer à la cellule suivante.
        else
            Supprimer (Lca.all.Suivant, Destination);
        end if;
    end Supprimer;


    procedure Vider (Lca : in out T_LCA_IP) is
    begin
        while not Est_Vide(Lca) loop
            Supprimer(Lca, Lca.all.Destination);
        end loop;
    end Vider;


    procedure Pour_Chaque (Lca : in T_LCA_IP) is

        procedure Traitement (Lca : in T_LCA_IP) is   -- La procedure qui empeche l'arret en cas d'erreur
        begin
            Traiter (Lca.all.Destination, Lca.all.Masque, Lca.all.Port, Lca.all.Frequence);
        exception
            when others => null;
        end Traitement;

    begin
        if not Est_Vide(Lca) then
            Traitement (Lca);
            -- Passer à la cellule suivante.
            Pour_Chaque (Lca.all.Suivant);
        else
            null;
        end if;
    end Pour_Chaque;


end LCA_IP;
