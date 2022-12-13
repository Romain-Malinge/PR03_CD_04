package Routeur_Exceptions is

    Destination_Absente_Exception : Exception;	-- une clé est absente d'un Lca
    Taille_Cache_Exception : Exception;          -- la taille du chache est <1
    Not_Txt_Exception : Exception;               -- les nom de fichiers ne sont pas en .txt
    Not_Politique_Exception : Exception;         -- la politique n'est pas accepter
    Table_Not_Found_Exception : Exception;       -- le fichier table n'est pas présent
    Paquet_Not_Found_Exception : Exception;      -- le fichier cache n'est pas présent
    Table_Invalide_Exception : Exception;        -- le fichier table n'est pas au normes
    Paquet_Invalide_Exception : Exception;       -- le fichier cache n'est pas au normes

end Routeur_Exceptions;
