\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"From far away"}}
  poet = \markup\oldStyleNum"William Morris (1834–1896)"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #138
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key bes \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 3 {
    \partial 8 f8^\mf |
    \slurDashed
    f( f) bes8 d8( d) c8 |
    bes[ a] g f4 \bar"" f8^\pp |
    bes bes bes a a a |
    
    a a a a4 \bar"" f8^\mf |
    f f bes d8( d) c8 |
    bes( a) g c4. | 
    b8^\p b b c[ d] c |
    
    bes a g f4. |
    \once\override DynamicText.X-offset = #'-3
    f^>^\f f8 f f |
    f4 bes8\rest bes4\rest  \bar""
    f8^\mf |
    bes( bes) bes8 a8( a) a8 |
    
    g4 g8 f4 f8 |
    ees ees ees d[ c] bes |
    f'( f) f8 f4 \bar"" f8 |
    
    d'( d) d8 f8( f) f8 |
    bes,4 bes8 d4 \bar"" d8 |
    g, g a bes( bes) ees8 |
    
    \slurSolid
    d4.(~ d8[ c bes] |
    c4.~ c8[ bes]) a |
    bes4.~^\markup\italic"dim." bes~ |
    \partial 8*5 bes~ bes4 \break
  }
  
  
  %2ndPage
  \repeat volta 3 {
    \partial 8 f8^\mf |
    \slurDashed
    f( f) bes8 d8( d) c8 |
    bes[ a] g f4 \bar"" f8^\pp |
    bes bes bes a a a |
    
    a a a a4 \bar"" f8^\mf |
    f( f) bes d8( d) c8 |
    bes[ a] g c4. | 
    b8^\p b b c[ d] c |
    
    bes a g f4. |
    \once\override DynamicText.X-offset = #'-3
    f^>^\f f8 f f |
    f4 bes8\rest bes4\rest  \bar""
    f8^\mf |
    bes( bes) bes8 a8( a) a8 |
    
    g4 g8 f4 f8 |
    ees( ees) ees d( c) bes |
    f'4 f8 f4 \bar"" f8 |
    
    d'( d) d8 f8( f) f8 |
    bes,4 bes8 d4 \bar"" d8 |
    g,( g) a bes( bes) ees8 |
    
    \slurSolid
    d4.(~ d8[ c bes] |
    c4.~ c8[ bes]) a |
    bes4.~^\markup\italic"dim." bes~ |
    \partial 8*5 bes~ bes4 \break
  }
  
  
  
  
  %3rdPage
  \repeat volta 3 {
    \slurDashed
    \partial 8 f16(^\mf f) |
    f8( f) bes8 d8( d) c8 |
    bes( a) g f4 \bar"" f8^\pp |
    bes bes bes a a a |
    
    a a a a4 \bar"" f8^\mf |
    f( f) bes d8( d) c8 |
    bes[ a] g c4. | 
    b8^\p b b c[ d] c |
    
    bes a g f4. |
    \once\override DynamicText.X-offset = #'-3
    f^>^\f f8 f f |
    f4 bes8\rest bes4\rest  \bar""
    f16(^\mf f) |
    bes8( bes) bes8 a8( a) a8 |
    
    g( g) g8 f4 f8 |
    ees( ees) ees d( c) bes |
    f'4 f8 f4 \bar"" f16( f) |
    
    d'8( d) d8 f8( f) f8 |
    bes,( bes) bes8 d4 \bar"" d8 |
    g,( g) a bes( bes) ees8 |
    
    \slurSolid
    d4.(~ d8[ c bes] |
    c4.~ c8[ bes]) a |
    bes4.~^\markup\italic"dim." bes~ |
    \partial 8*5 bes~ bes4 \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f8 |
  \slurDashed
  f( f) f8 f8( f) f8 |
  g[ f] ees d4 f8 |
  f g f f f f |
  
  g f e f4 f8 |
  d d d d( d) e8 |
  e( e) e8 f4. |
  f8 f f f4 f8 |
  
  f f e f4. |
  f f8 ees d |
  c4 s4. f8 |
  bes( bes) bes8 a( a) a8 |
  
  g4 g8 f4 f8 |
  ees ees ees d[ c] bes |
  f'( f) f8 f4 f8 |
  
  bes( bes) bes8 a( a) a8 |
  g4 g8 fis4 fis!8 |
  g g ees d( d) g8 |
  
  \slurSolid
  f4.~ f( |
  ees)~ ees4 ees8 |
  d4.( ees4 c8 |
  d4.)~ d4
  
  
  
  
  
  
  
  
  %2ndPageAlto
  f8 |
  \slurDashed
  f( f) f8 f8( f) f8 |
  g[ f] ees d4 f8 |
  f g f f f f |
  
  g f e f4 f8 |
  d( d) d d( d) e8 |
  e4 e8 f4. |
  f8 f f f4 f8 |
  
  f f e f4. |
  f f8 ees d |
  c4 s4. f8 |
  bes( bes) bes8 a( a) a8 |
  
  g4 g8 f4 f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f8 |
  
  bes( bes) bes8 a( a) a8 |
  g4 g8 fis4 fis!8 |
  g( g) ees d( d) g8 |
  
  \slurSolid
  f4.~ f( |
  ees)~ ees4 ees8 |
  d4.( ees4 c8 |
  d4.)~ d4
  
  
  
  
  
  
  
  
  
  
  
  %3rdPageAlto
  \slurDashed
  f16( f) |
  f8( f) f8 f8( f) f8 |
  g( f) ees d4 f8 |
  f g f f f f |
  
  g f e f4 f8 |
  d( d) d d( d) e8 |
  e4 e8 f4. |
  f8 f f f4 f8 |
  
  f f e f4. |
  f f8 ees d |
  c4 s4. f16( f) |
  bes8( bes) bes8 a( a) a8 |
  
  g( g) g8 f4 f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f16( f) |
  
  bes8( bes) bes8 a( a) a8 |
  g( g) g8 fis4 fis!8 |
  g( g) ees d( d) g8 |
  
  \slurSolid
  f4.~ f( |
  ees)~ ees4 ees8 |
  d4.( ees4 c8 |
  d4.)~ d4
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  From far __ a -- way __ we come to you,
  
  %\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  To tell of great ti -- \set associatedVoice = "sopranos"
  dings strange and true,
  \unset associatedVoice
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  From far __ a -- way __ we come to you,
  To tell of great ti -- dings strange and true,
  From far __ a -- way __ we come to you,
  To tell of great ti -- \set associatedVoice = "sopranos"
  dings strange __ and true. __
  \unset associatedVoice
  
  
  
  \set stanza = #"4. "
  "" “O __ ye \set ignoreMelismata = ##t shep -- herds, \unset ignoreMelismata what have ye seen,
  
  %\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  To slay __ your \set ignoreMelismata = ##t sor -- row \unset ignoreMelismata and heal your teen?”
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  \skip1 “O __ ye \set ignoreMelismata = ##t shep -- herds, \unset ignoreMelismata what have ye seen,
  To slay __ your \set ignoreMelismata = ##t sor -- row \unset ignoreMelismata and heal your teen?”
  \skip1 “O __ ye \set ignoreMelismata = ##t shep -- herds, \unset ignoreMelismata what have ye seen,
  To slay __ your \set ignoreMelismata = ##t sor -- row \unset ignoreMelismata and heal __ your teen?” __
  
  
  
  
  \set stanza = #"7. "
  And as __ we gazed this thing up -- on,
  
  %%\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  \set ignoreMelismata = ##t
  \skip1 Those twain knelt down to the lit -- tle One,
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  And _ as __ _ we gazed _ this thing _ up -- on,
  \skip1 Those twain knelt down to the lit -- tle One,
  And _ as __ _ we gazed _ this thing _ up -- on,
  \skip1 Those twain knelt down to
  \unset ignoreMelismata
  the lit -- tle One. __
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic 
  \set stanza = #"2. "
  For as __ we wan -- dered far __ and wide,
  
  \set associatedVoice = "sopranos"
  \markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \unset associatedVoice
  
  What hap do you \set ignoreMelismata = ##t deem there \unset ignoreMelismata should us __ be -- tide?
  
  \markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor,
  \set associatedVoice = "sopranos"
  \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \unset associatedVoice
  %\repeat unfold 14{\skip1}
  
  For as __ we wan -- dered far and wide,
  What hap do you \set ignoreMelismata = ##t deem there \unset ignoreMelismata should us __ be -- tide?
  For as __ we wan -- dered far and wide,
  What hap do you \set ignoreMelismata = ##t deem there \unset ignoreMelismata \set associatedVoice = "sopranos"
  should us __ be -- tide? __
  \unset associatedVoice
  
  
  
  \set stanza = #"5. "
  "" \set ignoreMelismata = ##t
  “In an ox -- stall __ _ this night _ we saw,
  
  \set associatedVoice = "sopranos"
  \markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \unset associatedVoice
  
  A Babe and a Maid __ _ with -- out a flaw,
  
  \markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor,
  \set associatedVoice = "sopranos"
  \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \unset associatedVoice
  %\repeat unfold 14{\skip1}
  
  \skip1 “In an ox -- stall __ _ this night we saw,
  A Babe and a Maid __ _ with -- out a flaw,
  \skip1 “In an ox -- stall __ _ this night we saw,
  A Babe and a \unset ignoreMelismata
  Maid __ with -- out __ a flaw. __
  
  
  
  
  \set stanza = #"8. "
  \set ignoreMelismata = ##t
  And a mar -- vel -- lous song _ we straight _ did hear,
  
  \set associatedVoice = "sopranos"
  \markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \unset associatedVoice
  
  That slew __ _ our sor -- row and healed our care,”
  
  \markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor,
  \set associatedVoice = "sopranos"
  \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \unset associatedVoice
  %\repeat unfold 14{\skip1}
  
  And a mar -- vel -- lous song _ we straight _ did hear,
  That slew __ _ our sor -- row and healed our care,”
  And a mar -- vel -- lous song _ we straight _ did hear,
  That slew __ _ our sor -- row
  \unset ignoreMelismata
  and healed __ our care,” __
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \skip1
  \set ignoreMelismata = ##t
  Un -- der a bent when the night _ was deep,
  
  %%\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  \skip1 There lay three shep -- _
  \set associatedVoice = "sopranos"
  herds tend -- ing their sheep,
  \unset ignoreMelismata
  \unset associatedVoice
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  \skip1 \set ignoreMelismata = ##t
  Un -- der a bent when the night was deep,
  \skip1 There lay three shep -- _ herds tend -- ing their sheep,
  \skip1 Un -- der a bent when the night was deep,
  \skip1 There lay three shep -- _ herds
  \set associatedVoice = "sopranos"
  tend -- _ _ _ _ _ ing
  \unset ignoreMelismata
  their sheep. __
  \unset associatedVoice
  
  
  
  \set stanza = #"6. "
  There was __ an old __ man there be -- side;
  
  %%\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  \set ignoreMelismata = ##t
  His hair __ _ was white, and his hood was wide,
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  There was __ _ an old __ _ man there be -- side;
  His hair __ _ was white, and his hood was wide,
  There was __ _ an old __ _ man there be -- side;
  His hair __ _ was white, and
  \unset ignoreMelismata
  his hood __ was wide. __
  
  
  
  
  \set stanza = #"9. "
  \set ignoreMelismata = ##t
  "" "" News of a fair and a mar -- vel -- lous thing,
  
  %%\markup\italic The \markup\italic snow \markup\italic in \markup\italic the \markup\italic street, \markup\italic and \markup\italic the \markup\italic wind \markup\italic on \markup\italic the \markup\italic door,
  \repeat unfold 11 \skip1
  
  No -- ël, __ _ No -- ël, __ _ No -- ël, we sing!
  
  %\markup\italic Min -- \markup\italic strels \markup\italic and \markup\italic maids \markup\italic stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor, \markup\italic Stand \markup\italic forth \markup\italic on \markup\italic the \markup\italic floor.
  \repeat unfold 14{\skip1}
  
  "" "" News of a fair and a mar -- vel -- lous thing,
  No -- ël, __ _ No -- ël, __ _ No -- ël, we sing!
  "" "" News of a fair and a mar -- vel -- lous thing,
  \unset ignoreMelismata
  No -- ël, __ No -- ël, __ No -- ël, __ we sing! __
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}
altoWordsVIII = \lyricmode {
}
altoWordsIX = \lyricmode {
}

tenorMusic = \relative c {
  d'8_\mf |
  \slurDashed
  d( d) d8 bes( bes) bes8 |
  bes4 bes8 d4 d8_\pp |
  d d d d cis d |
  
  e d cis d4 d8_\mf |
  bes bes bes bes( bes) c8 |
  des( des) des8 c[ bes a] |
  aes_\p g aes a4 a8 |
  
  g a bes a4. |
  \once\override DynamicText.X-offset = #'-3
  f4._\f d'8 c bes |
  a4 s4. f8_\mf |
  bes( bes) bes8 a8( a) a8 |
  
  g4 g8 f4 f8 |
  ees ees ees d[ c] bes |
  f'( f) f8 f4 f'8 |
  
  d( ees) f ees( d) c |
  bes[ c] d c[ bes] a |
  g g fis g( g) bes8 |
  
  \slurSolid
  bes4( c8 d4. |
  a4 bes8 c4) c8 |
  bes4.( g4 ees8 |
  f4.)~ f4
  
  
  
  
  
  
  %2ndPageTenor
  d'8_\mf |
  \slurDashed
  d( d) d8 bes( bes) bes8 |
  bes4 bes8 d4 d8_\pp |
  d d d d cis d |
  
  e d cis d4 d8_\mf |
  bes( bes) bes bes( bes) c8 |
  des4 des8 c[ bes a] |
  aes_\p g aes a4 a8 |
  
  g a bes a4. |
  \once\override DynamicText.X-offset = #'-3
  f4._\f d'8 c bes |
  a4 s4. f8_\mf |
  bes( bes) bes8 a8( a) a8 |
  
  g4 g8 f4 f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f'8 |
  
  d( ees) f ees( d) c |
  bes[ c] d c[ bes] a |
  g( g) fis g( g) bes8 |
  
  \slurSolid
  bes4( c8 d4. |
  a4 bes8 c4) c8 |
  bes4.( g4 ees8 |
  f4.)~ f4
  
  
  
  
  
  
  
  
  %3rdPageTenor
  \slurDashed
  d'16(_\mf d) |
  d8( d) d8 bes( bes) bes8 |
  bes8( bes) bes8 d4 d8_\pp |
  d d d d cis d |
  
  e d cis d4 d8_\mf |
  bes( bes) bes bes( bes) c8 |
  des4 des8 c[ bes a] |
  aes_\p g aes a4 a8 |
  
  g a bes a4. |
  \once\override DynamicText.X-offset = #'-3
  f4._\f d'8 c bes |
  a4 s4. f16(_\mf f) |
  bes8( bes) bes8 a8( a) a8 |
  
  g8( g) g8 f4 f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f'16( f) |
  
  d8( ees) f ees( d) c |
  bes( c) d c[ bes] a |
  g( g) fis g( g) bes8 |
  
  \slurSolid
  bes4( c8 d4. |
  a4 bes8 c4) c8 |
  bes4.( g4 ees8 |
  f4.)~ f4
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  bes'8 |
  \slurDashed
  bes( bes) bes8 bes,( bes) d8 |
  ees4 g8 bes4 a8 |
  g g g a a a |
  
  a a a d4 a8 |
  bes bes a g( g) g8 |
  g( a) bes a[ g f] |
  d d d c4 c8 |
  
  c c c f4. |
  f f8 f f |
  f4 d8\rest d4\rest f8 |
  bes( bes) bes8 a( a) a8 |
  
  g4 g8 f4 f8 |
  ees ees ees d[ c] bes |
  f'8( f) f8 f4 f8 |
  
  bes( bes) bes8 f( f) f8 |
  g4 g8 d4 d8 |
  ees ees c g'( g) ees8 |
  
  f4.~ f~ |
  f~ f4 f8 |
  bes,4.~ bes~ |
  bes~ bes4
  
  
  
  
  
  
  %2ndPageBass
  bes'8 |
  \slurDashed
  bes( bes) bes8 bes,( bes) d8 |
  ees4 g8 bes4 a8 |
  g g g a a a |
  
  a a a d4 a8 |
  bes( bes) a g( g) g8 |
  g[ a] bes a[ g f] |
  d d d c4 c8 |
  
  c c c f4. |
  f f8 f f |
  f4 d8\rest d4\rest f8 |
  bes( bes) bes8 a( a) a8 |
  
  g4 g8 f4 f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f8 |
  
  bes( bes) bes8 f( f) f8 |
  g4 g8 d4 d8 |
  ees( ees) c g'( g) ees8 |
  
  f4.~ f~ |
  f~ f4 f8 |
  bes,4.~ bes~ |
  bes~ bes4
  
  
  
  
  
  %3rdPageBass
  \slurDashed
  bes'16( bes) |
  bes8( bes) bes8 bes,( bes) d8 |
  ees8( ees) g8 bes4 a8 |
  g g g a a a |
  
  a a a d4 \bar"" a8 |
  bes( bes) a g( g) g8 |
  g[ a] bes a[ g f] |
  d d d c4 c8 |
  
  c c c f4. |
  f f8 f f |
  f4 d8\rest d4\rest f16( f) |
  bes8( bes) bes8 a( a) a8 |
  
  g( g) g8 f4 \bar"" f8 |
  ees( ees) ees d( c) bes |
  f'4 f8 f4 f16( f) |
  
  bes8( bes) bes8 f( f) f8 |
  g( g) g8 d4 d8 |
  ees( ees) c g'( g) ees8 |
  
  f4.~ f~ |
  f~ f4 f8 |
  bes,4.~ bes~ |
  bes~ bes4
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIX"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIX
    \new Lyrics = "altosVIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVIII
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
%6.14 \context {\Lyrics\override LyricText.font-size = #0.9 }
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/4)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

