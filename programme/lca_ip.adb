with Ada.Text_IO;            use Ada.Text_IO;
with Routeur_Exceptions;     use Routeur_Exceptions;
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


    function Est_Trie (Lca : in T_LCA_IP) return Boolean is
    begin
        if Est_Vide(Lca) then
            return True;
        elsif not Est_Vide(Lca.all.Suivant) and then Lca.all.Masque < Lca.all.Suivant.all.Masque then
            return False;
        else
            return Est_Trie (Lca.all.Suivant);
        end if;
    end Est_Trie;


    procedure Trie (Lca : in out T_LCA_IP) is
        IP_Mem : T_Adresse_IP;
        Port_Mem : Unbounded_String;


        procedure Trie_Echange (Lca : T_LCA_IP) is
        begin
            if Est_Vide(Lca) then
                null;
            elsif not Est_Vide(Lca.all.Suivant) and then Lca.all.Masque < Lca.all.Suivant.all.Masque then
                -- Echange les Destinations
                IP_Mem := Lca.all.Destination;
                Lca.all.Destination := Lca.all.Suivant.all.Destination;
                Lca.all.Suivant.all.Destination := IP_Mem;
                -- Echange les Masques
                IP_Mem := Lca.all.Masque;
                Lca.all.Masque := Lca.all.Suivant.all.Masque;
                Lca.all.Suivant.all.Masque := IP_Mem;
                -- Echange les interfaces
                Port_Mem := Lca.all.Port;
                Lca.all.Port := Lca.all.Suivant.all.Port;
                Lca.all.Suivant.all.Port := Port_Mem;
                -- Echange Suivant
                Trie_Echange (Lca.all.Suivant);
            else
                Trie_Echange (Lca.all.Suivant);
            end if;
        end Trie_Echange;

    begin
        while not Est_Trie(Lca) loop
            Trie_Echange(Lca);
        end loop;
    end Trie;


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


    function La_Frequence_Premier (Lca : in out T_LCA_IP) return Integer is
    begin
        -- Creer une nouvelle cellule.
        if Est_Vide(Lca) then
            return 0;
        else
            return Lca.all.Frequence;
        end if;
    end La_Frequence_Premier;


    procedure Trouver_Grand_Masque (Lca : in out T_LCA_IP;
                                    Destination : in T_Adresse_IP;
                                    IP : in T_Adresse_IP;
                                    Grand_Masque : in out T_Adresse_IP) is
    begin
        if Est_Vide(Lca) then
            null;
        elsif (Lca.all.Destination and Grand_Masque) = (IP and Grand_Masque) and Grand_Masque < Lca.all.Masque then
            Grand_Masque := Lca.all.Masque;
            Trouver_Grand_Masque (Lca.all.Suivant, Destination, IP, Grand_Masque);
        else
            Trouver_Grand_Masque (Lca.all.Suivant, Destination, IP, Grand_Masque);
        end if;
    end Trouver_Grand_Masque;


    procedure Enregistrer (Lca : in out T_LCA_IP;
                           Destination : in T_Adresse_IP;
                           Masque : in T_Adresse_IP;
                           Port : in Unbounded_String;
                           Frequence : in Integer := 0) is
        New_Cel : T_LCA_IP;
    begin
        -- Creer une nouvelle cellule
        if Est_Vide(Lca) then
            New_Cel := new T_Cellule'(Destination, Masque, Port, Frequence, Lca);
            Lca := New_Cel;
            -- Modifier la donnee
        elsif Lca.all.Destination = Destination then
            Lca.all.Masque := Masque;
            Lca.all.Port := Port;
            Lca.all.Frequence := Frequence;
            -- Passer à la cellule suivante
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
        else
            return Destination_Presente (Lca.all.Suivant, Destination);
        end if;
    end;


    procedure Supprimer (Lca : in out T_LCA_IP;
                         Destination : in T_Adresse_IP) is
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
            -- Passer à la cellule suivante
        else
            Supprimer (Lca.all.Suivant, Destination);
        end if;
    end Supprimer;


    procedure Supprimer_Premier (Lca : in out T_LCA_IP) is
    begin
        -- Lever l'exeption.
        if Est_Vide(Lca) then
            null;
            -- Supprimer la première cellule sans laisser de miettes
        else
            Supprimer (Lca, Lca.all.Destination);
        end if;
    end Supprimer_Premier;


    procedure Supprimer_LFU (Lca : in out T_LCA_IP; Dernier_Ajout : in T_Adresse_IP) is

        -- Variables
        Frequence_Min : Integer;       -- La fréquence d'utilisation min
        Des_Supr : T_Adresse_IP;       -- La destination à supprimer

        -- Procedures
        procedure Trouver_Min (Lca : in T_LCA_IP; Frequence_Min : in out Integer) is
        begin
            if Est_Vide(Lca) then
                null;
            elsif Lca.all.Frequence <= Frequence_Min and Lca.all.Destination /= Dernier_Ajout then
                Des_Supr := Lca.all.Destination;
                Frequence_Min := Lca.all.Frequence;
                Trouver_Min(Lca.all.Suivant, Frequence_Min);
            else
                Trouver_Min(Lca.all.Suivant, Frequence_Min);
            end if;
        end Trouver_Min;



    begin
        Frequence_Min := La_Frequence_Premier(Lca);
        Trouver_Min (Lca, Frequence_Min);
        Supprimer (Lca, Des_Supr);
    end Supprimer_LFU;


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
