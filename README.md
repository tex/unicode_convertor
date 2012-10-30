unicode_convertor
=================

Set of functions to convert any character set into unicode and back,
to convert any capital latin character to small latin character and back,
to remove accent from any latin character

Author:

    Milan Svoboda (milan.svoboda@centrum.cz)

Main function:

    unicode_convert:map(To, {From, Input}) -> Output
        Converts Input in code page specified in From to Output in code page
        specified in Output.

        To - The code page of the output
        From - the code page of the input
        Input - Input

    unicode_convert:to_upper(Code, Input) -> Output
        Converts small latin characters in Input in code page specified
        in Code to capital latin characters.

    unicode_convert:to_lower(Code, Input) -> Output
        Converts capital latin characters in Input in code page specified
        in Code to small latin characters.

    unicode_convert:remove_accent(Code, Input) -> Output
        Removes accent from latin characters in Input in code page specified
        in Code.

    To, From, Code - Atom 'unicode', 'iso-8859-??', 'cp1250', ...

Todo:

- Fix the 'Warning: this clause cannot match because a previous clause at line XXX always matches'
  This is caused by some Microsoft Windows mappings that maps one unicode character to
  more than one characters.

  Fixed by removing support to map from unicode to Microsoft Windows mappings.
  There was bigger problem of unknown reason that has been fixed with this too. The thing
  is that m() throws badarg for any (unicode, {cp..., ...}) or (cp..., {unicode, ...}).

Thanks to Witold Baryluk (baryluk) for his project to_unicode
(https://github.com/baryluk/to_unicode) that served as inspiration for me.
