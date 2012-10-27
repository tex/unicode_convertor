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

    unicode_convert:to_capital(Code, Input) -> Output
        Converts small latin characters in Input in code page specified
        in Code to capital latin characters.

    unicode_convert:to_small(Code, Input) -> Output
        Converts capital latin characters in Input in code page specified
        in Code to small latin characters.

    unicode_convert:remove_accent(Code, Input) -> Output
        Removes accent from latin characters in Input in code page specified
        in Code.

    To, From, Code - Atom 'unicode', 'iso-8859-??', ...

Thanks to Witold Baryluk (baryluk) for his project to_unicode
(https://github.com/baryluk/to_unicode) that served as inspiration for me.
