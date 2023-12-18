\version "2.24.0"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \concat{\italic "HymnsAndCarolsOfChristmas.com"}}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -12)
       (stretchability . 100))
  %markup-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -5)
  %     (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing.stretchability = 50
  top-markup-spacing.stretchability = 30
  last-bottom-spacing.stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #092
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
  \key g \major
  \time 4/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \autoBeamOff
}

sopMusic = \relative c'' {
  \partial 2
  \repeat unfold 2 {
    d2 |
    d4 d e d |
    d2 b |
    c b4 a~ |
    a g2 fis4 |
    g2
  }
  b4\rest b4 |
  a fis g e |
  d2 b'4\rest d4 |
  d d e d |
  d2 b |
  c b4\! a~ |
  a g2 fis4 |
  g2 \bar "|."
}
sopWords = \lyricmode {
  \repeat unfold 41 { \skip 1 }
  der halb -- en Nacht.
}

altoMusic = \relative c'' {
  \repeat unfold 2 {
    b2 |
    b4 g g g |
    fis2 e |
    e d4 d |
    e4.( b8 d4) d4 |
    d2
  }
  s4 g |
  e d d cis |
  d8[ e]( fis4) s fis |
  a g g g |
  fis2 e |
  e g4( e |
  fis4) g a d, |
  d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = "1. "
  Es ist ein Ros ent -- sprung -- en,
  aus ein -- er Wur -- zel zart,
  wie uns die Alt -- en sung -- en,
  von Jes -- se kam __ die Art
  \set associatedVoice = "tenors"
  Und hat ein Blüm -- lein bracht
  \unset associatedVoice
  mit -- ten im kalt -- en Win -- ter,
  wohl zu __ der halb -- en Nacht.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = "2. "
  Das Rös -- lein, das ich mein -- e,
  da -- von Je -- sai -- a sagt,
  ist Ma -- ri -- a die rei -- ne
  die uns das Blüm -- lein bracht.
  \set associatedVoice = "tenors"
  Aus Got -- tes ew’ -- gem Rat
  \set associatedVoice = "sopranos"
  hat sie ein Kind ge -- bor -- en
  und blieb ein rei -- ne Magd.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = "3. "
  Das Blüm -- e -- lein, so klein -- e,
  das duf -- tet uns __ so süß,
  mit sein -- em hel -- len Schein -- e
  ver -- treibt’s die Fin -- ster -- nis.
  \set associatedVoice = "tenors"
  Wahr Mensch und wahr -- er Gott,
  \set associatedVoice = "sopranos"
  hilft uns aus al -- lem Leid -- e,
  ret -- tet von Sünd und Tod.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \repeat unfold 2 {
    d2 |
    d4 b c b |
    a2 g |
    g b4 d |
    c( b2) a4 |
    b2
  }
  s4 d |
  c b b a |
  fis8[ g]( a4) s4 a4 |

  a b c b |
  a2 gis |
  a d4 c |
  b2 a |
  b \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat unfold 2 {
    g'2 |
    g4 g c g |
    d2 e |
    c g'4 fis |
    e2 d |
    g,
  }
  d'4\rest g |
  a b g a |
  d,2 d4\rest d |
  fis g c, g' |
  d2 e |
  a, b4 c |
  d2 d |
  g, \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  Wur -- zel zart,
  \repeat unfold 10 { \skip 1 }
  war die Art
  \repeat unfold 16 { \skip 1 }
  halb -- en Nacht.
}
bassWordsII = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  -sai -- a sagt,
  \repeat unfold 10 { \skip 1 }
  Blüm -- lein bracht.
  \repeat unfold 16 { \skip 1 }
  rei -- ne Magd.
}
bassWordsIII = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  uns so süß,
  \repeat unfold 10 { \skip 1 }
  Fin -- ster -- nis.
  \repeat unfold 16 { \skip 1 }
  Sünd und Tod.
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
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . 0.2)) } \lyricsto "tenors" \tenorWords
%{IF_LESSER
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsIII
%}%END_IF_LESSER
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    %#(layout-set-staff-size 15)
    %#(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20)))
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Es ist ein Ros entsprungen"}}
    poet = \markup\oldStyleNum"15th Century German"
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }
}








%IF_NOT_LESSER

sopWords = \lyricmode {
  \repeat unfold 41 { \skip 1 }
  flos or -- tus est.
}

altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = "1. "
  Flos de ra -- dí -- ce Jes -- se, est na -- tus hó -- di -- e.
  Quem no -- bis jam ad -- és -- se, læ -- tá -- mur ú -- ni -- ce.
  \set associatedVoice = "tenors"
  Flos il -- le Je -- sus est.
  \unset associatedVoice
  Ma -- rí -- a Vir -- go ra -- dix de qua __ flos or -- tus est.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = "2. "
  Hunc I -- sa -- í -- as flo -- rem, præ -- sá -- giis cé -- ci -- nit.
  Ad e -- jus nos a -- mó -- rem, Na -- scén -- tis ál -- li -- cit.
  \set associatedVoice = "tenors"
  Flos vir -- gam sú -- per -- at
  \set associatedVoice = "sopranos"
  cæ -- li ter -- rǽ -- que ci -- ves, Flos il -- le ré -- cre -- at.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = "3. "
  Est cam -- pi flos pu -- dí -- ci, est flos con -- vál -- li -- um.
  Pul -- chrúm -- que pot -- est di -- ci, in spi -- nis lí -- li -- um.
  \set associatedVoice = "tenors"
  O -- dó -- ris óp -- ti -- mi;
  \set associatedVoice = "sopranos"
  vel so -- li quod -- vis ce -- dit a -- ró -- ma nó -- mi -- ni.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = "4. "
  Hic su -- o flos o -- dó -- re, fi -- dé -- les át -- tra -- hit. 
  Di -- ví -- no mox a -- mó -- re, at -- trác -- tos ím -- bu -- it.
  \set associatedVoice = "tenors"
  O flos o grá -- ti -- a:
  \set associatedVoice = "sopranos"
  ad Te, ad Te su -- spí -- ro, de Te me sá -- ti -- a.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}

bassWords = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  hó -- di -- e.
  \repeat unfold 10 { \skip 1 }
  ú -- ni -- ce.
  \repeat unfold 16 { \skip 1 }
  or -- tus est.
}
bassWordsII = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  cé -- ci -- nit.
  \repeat unfold 10 { \skip 1 }
  ál -- li -- cit.
  \repeat unfold 16 { \skip 1 }
  ré -- cre -- at.
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
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
%6.14g    \context { \Lyrics \override LyricText.font-size = #0.6 }
%6x9 \context {\Lyrics\override LyricText.font-size = #1.2 }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Flos de radice Jesse"}}
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    %tagline = \markup { "from" \italic {HymnsAndCarolsOfChristmas.com}}
  }
}
%END_IF_NOT_LESSER







sopWords = \lyricmode {
  \repeat unfold 41 { \skip 1 }
  -spent was the night.
}



altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = "1. "
  Lo, how a Rose e’er bloom -- ing
  From ten -- der stem __ has sprung!
  Of Jes -- se’s lin -- eage com -- ing
  As men of old __ have sung.
  \set associatedVoice = "tenors"
  It came, a flow’r -- et bright,
  \unset associatedVoice
  A -- mid the cold of win -- ter
  When half -- spent was the night.
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{"2. "}
  \lyricmode {
    I -- sa -- iah ’twas fore -- told it,
    The Rose I had __ in mind;
    With Ma -- ry we be -- hold it,
    The Vir -- gin Moth -- er kind.
    \set associatedVoice = "tenors"
    To show God’s love a -- right __
    \set associatedVoice = "sopranos"
    She bore to men a Sav -- ior,
    When half -- spent was the night.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = "3. "
  The shep -- herds heard the sto -- ry
  Pro -- claimed by an -- gels bright,
  How Christ, the Lord of Glo -- ry
  Was born on earth __ this night.
  \set associatedVoice = "tenors"
  To Beth -- le -- hem they sped __
  \set associatedVoice = "sopranos"
  And in the man -- ger found Him,
  As an -- gel her -- alds said.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = "4. "
  O Flow’r, whose fra -- grance ten -- der
  With sweet -- ness fills __ the air,
  Dis -- pel with glo -- rious splen -- dor
  The dark -- ness ev -- ’ry -- where;
  \set associatedVoice = "tenors"
  True man, yet ve -- ry God, __
  \set associatedVoice = "sopranos"
  From Sin and death now save us,
  And share our ev -- ’ry load.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}

bassWords = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  stem has sprung!
  \repeat unfold 10 { \skip 1 }
  old have sung.
  \repeat unfold 16 { \skip 1 }
  was the night.
}
bassWordsII = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  had in mind;
  \repeat unfold 10 { \skip 1 }
  Moth -- er kind.
%{IF_LESSER
  \repeat unfold 16 { \skip 1 }
  was the night.
%}%END_IF_LESSER
}
bassWordsIII = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  fills the air,
  \repeat unfold 10 { \skip 1 }
  ev -- ’ry -- where;
  \repeat unfold 16 { \skip 1 }
  ev -- ’ry load.
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
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "tenors" \tenorWords
%{IF_LESSER
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsIII
%}%END_IF_LESSER
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    %#(layout-set-staff-size 15)
    %#(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20)))
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Lo, How a Rose E’er Blooming"}}
    poet = \markup\oldStyleNum"15th Century German"
    meter = \markup\oldStyleNum"Translated by Theodore Baker (1851–1934)"
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }
}

