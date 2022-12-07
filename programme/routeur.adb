with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP; use LCA_IP;


procedure Routeur is

     --Définition des types secondaires
     type E_Politique is (FIFO,LRU,LFU);


     --Espace pour les sous-programmes



     Nom_Paquet : Unbounded_String;
     Nom_Table : Unbounded_String;
     Nom_Resultat : Unbounded_String;
     Taille_Cache : Integer;
     Politique : E_Politique;
     Fin : Boolean;
     Bavard : Boolean;
     Nbr_Ajouter : Interger;


begin





end
