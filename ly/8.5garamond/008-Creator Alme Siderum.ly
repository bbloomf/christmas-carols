\version "2.24.0"
\include "util.ly"
\version "2.24.0"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {tagline = ""}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #008
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \set Score.timing = ##f
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    a4 f a
    c c d bes
    c \bar "|"
    c d bes
    c bes a g
    a\fermata \bar "|"
    
    c bes g
    a bes a g
    f \bar "|"
    f a bes
    c bes a g
    a\fermata
  }
}
sopAmen = \relative c'' {
  a4( bes a) g2( a) \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 f f
  f f f d
  f
  a f d
  e d f e
  f \bar "|"
  
  a g e
  f g f e
  d
  d f g
  a g f e
  f
}
altoAmen = \relative c' {
  f2. e2( f)
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1."
  Cre -- á -- tor al -- me sí -- de -- rum,
  Æ -- tér -- na lux cre -- dén -- ti -- um,
  Je -- su, Red -- émp -- tor óm -- ni -- um,
  In -- tén -- de vo -- tis súp -- pli -- cum.

  \set stanza = #"4."
  Cu -- jus pot -- és -- tas gló -- ri -- æ,
  No -- mén -- que cum pri -- mum so -- nat,
  Et cǽ -- li -- tes et ín -- fe -- ri
  Tre -- mén -- te cur -- ván -- tur ge -- nu.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2."
  Qui dǽ -- mo -- nis ne fráu -- di -- bus
  Per -- í -- ret or -- bis, ím -- pe -- tu
  A -- mó -- ris ac -- tus, lán -- gui -- di
  Mun -- di me -- dé -- la fac -- tus es.

  \set stanza = #" 5."
  Te de -- pre -- cá -- mur úl -- ti -- mæ
  Ma -- gnum di -- é -- i Jú -- di -- cem,
  Ar -- mis su -- pér -- næ grá -- ti -- æ
  De -- fén -- de nos ab hós -- ti -- bus.

  A -- men.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  Com -- mú -- ne qui mun -- di ne -- fas
  Ut ex -- pi -- á -- res, ad cru -- cem
  E Vír -- gi -- nis sa -- crá -- ri -- o
  In -- tác -- ta prod -- is víc -- ti -- ma.

  \set stanza = #" 6."
  Vir -- tus, ho -- nor, laus, gló -- ri -- a
  De -- o Pa -- tri cum Fí -- li -- o,
  Sanc -- to si -- mul Pa -- rá -- cli -- to,
  In sæ -- cu -- ló -- rum sǽ -- cu -- la.
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \dropLyricsVII
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \dropLyricsVII
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  c4 a c
  a a bes g
  a
  c bes bes
  c bes c c
  c \bar "|"
  
  c c c
  c d c bes
  a
  a c bes
  a g a c
  c
}
tenorAmen = \relative c' {
  c2. c1
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 f f
  f f bes, g'
  f
  f bes, bes
  c bes c c
  f\fermata \bar "|"
  
  f c c
  f g c, c
  d
  d c bes
  f g d' c
  f\fermata
}
bassAmen = \relative c {
  f2. c2( f)
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
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global {\repeat unfold2 \sopMusic \sopAmen}>> }
      \new Voice = "altos" { \voiceTwo << \global {\repeat unfold2 \altoMusic \altoAmen} >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global {\repeat unfold2  \tenorMusic \tenorAmen} >> }
      \new Voice = "basses" { \voiceTwo << \global {\repeat unfold2 \bassMusic \bassAmen} >> }
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
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      \Staff
      \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
      \remove "Time_signature_engraver"
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Creator alme siderum"}}
    %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Vesper Hymn for Sundays in Advent)"}}
    %arranger = \markup\oldStyleNum"Edited by Benjamin Bloomfield (1984–)"
    tagline = \markup { "from" \italic "Peters’ Sodality Hymn Book," 1914, via \italic "books.google.com"}
  }
}
\markup\vspace#0.5

















global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \set Score.timing = ##f
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    a4 f a
    c c d bes
    c \bar "|"
    c d bes
    c bes a g
    a\fermata \bar "|"
    
    c bes g \bar""
    a bes \bar"" a g
    f \bar "|"
    f a bes
    c bes a g
    a\fermata
  }
}
sopAmen = \relative c'' {
  a4( bes a) g2( a) \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 f f
  f f f d
  f
  a f d
  e d f e
  f \bar "|"
  
  a g e
  f g f e
  d
  d f g
  a g f e
  f
}
altoAmen = \relative c' {
  f2. e2( f)
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1."
  Cre -- a -- tor of the stars of night,
  Thy peo -- ple’s ev -- er -- last -- ing Light;
  Je -- su, Re -- deem -- er, save us all,
  And hear thy ser -- vants when they call.

  \set stanza = #"4."
  At Whose dread Name, ma -- jes -- tic now,
  All knees must bend, all hearts must bow;
  And things ce -- les -- tial Thee shall own,
  And things ter -- res -- trial, Lord a -- lone.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2."
  Thou, griev -- ing that the an -- cient curse
  Should doom to death a u -- ni -- verse,
  Hast found the med -- ’cine, full of grace,
  To save and heal a ru -- in’d race.
  
  \set stanza = #" 5."
  O Thou, Whose com -- ing is with dread
  To judge and doom the quick and dead,
  Pre -- serve us, while we dwell be -- low,
  From ev -- ’ry in -- sult of the foe.

  A -- men.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  Thou cam’st, the Bride -- groom of the bride,
  As drew the world to eve -- ning -- tide;
  Pro -- ceed -- ing from a vir -- gin shrine,
  The spot -- less Vic -- tim all di -- vine.

  \set stanza = #" 6."
  To God the Fa -- ther, God the Son,
  And God the Spi -- rit, Three in One,
  Laud, hon -- or, might, and glo -- ry be
  From age to age e -- ter -- nal -- ly.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  c4 a c
  a a bes g
  a
  c bes bes
  c bes c c
  c \bar "|"
  
  c c c
  c d c bes
  a
  a c bes
  a g a c
  c
}
tenorAmen = \relative c' {
  c2. c1
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 f f
  f f bes, g'
  f
  f bes, bes
  c bes c c
  f\fermata \bar "|"
  
  f c c
  f g c, c
  d
  d c bes
  f g d' c
  f\fermata
}
bassAmen = \relative c {
  f2. c2( f)
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
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global {\repeat unfold2 \sopMusic \sopAmen}>> }
      \new Voice = "altos" { \voiceTwo << \global {\repeat unfold2 \altoMusic \altoAmen} >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global {\repeat unfold2  \tenorMusic \tenorAmen} >> }
      \new Voice = "basses" { \voiceTwo << \global {\repeat unfold2 \bassMusic \bassAmen} >> }
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
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      \Staff
      \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
      \remove "Time_signature_engraver"
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Creator of the Stars of Night"}}
    poet = \markup\oldStyleNum"Translated by John Mason Neale (1818–1866)"
    %arranger = \markup\oldStyleNum"Edited by Benjamin Bloomfield (1984–)"
    tagline = \markup { "from" \italic "Peters’ Sodality Hymn Book," 1914, via \italic "books.google.com"}
  }
}
\markup \fill-line{\concat{ "from " \italic "Peters’ Sodality Hymn Book" \oldStyleNum", 1914, via " \italic "books.google.com"}}
\markup\vspace#0




























%IF_NOT_6.14
global = {
  \key f \major
  \time 16/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    \partial 4*12 a2 f4 a c c d bes c2 b\rest |
    \partial 4*12 c4 d bes c c2( bes4) a g( a) a2 |
    \partial 4*11 c4 bes g bes a bes g f2 b\rest |
    \partial 4*15 f4 a bes c c2 bes( a4) g( a) a1
  }
}
sopWords = \lyricmode {
  \repeat unfold 11 \skip1
  lux cre -- dén -- ti -- um,
  
  \repeat unfold 11 \skip1
  \dropLyricsIV
  pre -- ces súp -- pli -- cum.
}

altoMusic = \relative c' {
  \partial 4*12 e2 d4 c e f f d e2 s |
  \partial 4*12 f4 f d c8([ d e f g f d e] f4) d e8 e f2 |
  \partial 4*11 e4 d e d f f e c2 s |
  \partial 4*15 d4 e g e f2 d8[( e] f4. e8 d4) f e1 \bar ":|."
}
altoWords = \lyricmode {
  \dropLyricsVI
  \set stanza = #"1."
  Cón -- di -- tor al -- me sí -- de -- rum,
  Æ -- tér -- na lux __ cre -- dén -- ti -- um,
  \dropLyricsIX
  Chris -- te, Red -- émp -- tor óm -- ni -- um,
  Ex -- áu -- di pre -- ces súp -- pli -- cum.
}
altoWordsII = \lyricmode {
%\markup\italic
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
  
}

tenorMusic = \relative c' {
  \partial 4*12 cis2 d4 a g a a g g2 s |
  \partial 4*12 a4 bes f a g4.( a8 bes4 c) d4. cis8 d2 |
  \partial 4*11 c4 f, c' bes d d c a2 s |
  \partial 4*15 bes4 c d c a2 bes8([ c] d4 c bes) d cis1 \bar ":|."
}
tenorWords = \lyricmode {
  \repeat unfold 11 \skip1
  \dropLyricsIV
  lux cre -- dén -- ti -- um,
  
}
%  \repeat unfold 11 \skip1
%  pre -- ces súp -- pli -- cum.

bassMusic = \relative c {
  \partial 4*12 a2 d4 f c f d g c,2 d\rest |
  \partial 4*12 f4 bes, bes f' c8[( d e f] g4) a( bes a8) a d,2 |
  \partial 4*11 a4 bes c g' d bes a f2 d'\rest |
  \partial 4*15 bes4 a g c, f2 g8([ f16 e d8 e] f4 g) d a'1 \bar ":|."
}
bassWords = \lyricmode {
  \repeat unfold 11 \skip1
  lux cre -- dén -- ti -- um,
  
}
%  \repeat unfold 11 \skip1
%  pre -- ces súp -- pli -- cum.

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      \Staff
      \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
      \remove "Time_signature_engraver"
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Conditor alme siderum"}}
    poet = \markup\oldStyleNum"Anonymous, 7th Century"
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    
    tagline = \markup{from \italic"cpdl.org"}
  }
}
%\markup\fill-line \center-align {\concat{"from "\italic"cpdl.org"}}
\markup\vspace#0
%END_IF_NOT_6.14


















global = {
  \key d \major
  \time 4/4
}

sopMusic = \relative c'' {
  d4^"I" d2 a4 |
  d d2 d4 |
  d d cis b |
  a1 \bar "||"
  
  d4.^"II" d,8 d4 e |
  fis d fis a |
  b2 a4( g) |
  fis1 \bar "||"
  
  fis4.^"III" g8 fis4 e |
  d d' a fis |
  g2 a |
  d,1 \bar "|."
}
sopWords = \lyricmode {
  \dropLyricsIV
  Christ -- mas is com -- ing! The goose is get -- ting fat;
  Please to put a pen -- ny in the old man’s hat,
  Please to put a pen -- ny in the old man’s hat.
}

\score {
  \new Staff = women << \new Voice = sopranos { \global \sopMusic }
    \new Lyrics \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Christmas is Coming"}}
    %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Three-part Round)"}}
    poet = \markup\oldStyleNum"Traditional"
    composer = \markup\oldStyleNum"Edith Nesbitt (1858–1924)"
  }
}

