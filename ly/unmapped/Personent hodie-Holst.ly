\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Personent hodie"}}
  poet = \markup \concat{"Words from " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
  composer = \markup\oldStyleNum"Arranged by Gustav Holst (1874–1934)"
  tagline = ""
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key ees \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  b'1\rest |
  b1\rest |
  \oneVoice
  f4 f c'2 |
  bes4 bes c2 |
  c4 c f2 |
  d4 ees c2 |
  
  bes4 c ees2 |
  bes4 c aes2 |
  g4 f g ees |
  f2 f |
  g4 aes bes ees, |
  f2 f |
  <ees g>4 <f aes> <bes g>2 |
  
  <bes g> <bes g> |
  <aes f>4 <bes g> <c aes>2 |
  <c aes> <c aes> |
  <g ees>4 <aes f> <bes g>2 |
  aes4 g f2 |
  g4 f f ees |
  f2 f \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Per -- so -- nent ho -- di -- e
  vo -- ces pu -- e -- ru -- læ,
  lau -- dan -- tes ju -- cun -- de
  Qui no -- bis est na -- tus,
  sum -- mo De -- o da -- tus,
  et de vir, vir, vir,
  et de vir, vir, vir,
  et de vir -- gi -- ne -- o
  ven -- tre pro -- cre -- a -- tus.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  In mun -- do na -- sci -- tur,
  pan -- nis in -- vol -- vi -- tur,
  præ -- se -- pi po -- ni -- tur
  sta -- bu -- lo bru -- to -- rum,
  rec -- tor su -- per -- no -- rum,
  per -- di -- dit, dit, dit,
  per -- di -- dit, dit, dit,
  per -- di -- dit spo -- li -- a
  prin -- ceps in -- fer -- no -- rum.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Ma -- gi tres ve -- ne -- runt,
  par -- vu -- lum in -- qui -- runt,
  Beth -- le -- hem ad -- e -- unt,
  stel -- lu -- lam se -- quen -- do,
  ip -- sum ad -- o -- ran -- do,
  au -- rum thus, thus, thus,
  au -- rum thus, thus, thus,
  au -- rum thus, et myr -- rham
  E -- i of -- fe -- ren -- do.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Om -- nes cle -- ri -- cu -- li,
  par -- i -- ter pu -- e -- ri,
  can -- tent ut an -- ge -- li:
  Ad -- ven -- is -- ti mun -- do,
  lau -- des Ti -- bi fun -- do.
  Id -- e -- o, o, o,
  id -- e -- o, o, o,
  id -- e -- o, glo -- ri -- a
  in ex -- cel -- sis De -- o.
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
  
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
 
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  r1 |
  r1 |
  <f c aes>4 q <c g' c>2 |
  <ees bes'>4 <ees g bes> <ees aes c>2 |
  <f aes c>4 q << <f f'>2 \\ {bes4 aes} >> |
  <g d'>4 << {<ees ees'> <ees c'>2} \\ {bes'4 bes aes} >> |
  
  <ees g bes>4 <ees aes c> <ees bes' ees>2 |
  <f bes>4 <ees g c> << <c aes'>2 \\ {g'4 f} >> |
  <g d bes>4 <f d> <g d bes> <ees c> |
  <f c aes>2 q |
  <g d g,>4 <aes ees c> <bes d, bes> <ees, c g> |
  <f d bes>2 q |
  << {b'4\rest <bes ees> <aes d> <g c>} \\ {<g, ees>4 <aes f> <bes g>2} >> |
  
  <<
    {
      <f' bes>4 <ees aes> <d g> <c f> |
      b'4\rest <bes ees> <aes d> <g c> |
      <f bes>4 <ees aes> <d g> <c f> |
      b'4\rest <aes ees'> <g d'> <ees bes'> |
      c'4 <c f> <aes ees'> <f c'> |
    } \\
    {
      <g, bes>2 q|
      <f aes>4 <g bes> <aes c>2 |
      q q |
      <g ees>4 <aes f> <bes g>2 |
      <aes f>4 <bes g> <c aes>2 |
    }
  >>
  <g bes ees g>4 <f f'> <f g c f> <ees ees'> |
  <f a c f>2 q \bar "||"
}
pianoLH = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  r4 <f f,> <ees ees,> <d d,> |
  <c c,> <bes bes,> <aes aes,> <g g,> |
  <f f,> <d d,> <ees ees,> <c c,> |
  <g g'> <ees ees'> <aes aes'> <g g'> |
  <f f'> <ees ees'> <d d'> <c c'> |
  <bes bes'> <g g'> <aes aes'> <c c'> |
  
  <ees ees'> <aes aes'> <g g'> <ees ees'> |
  <d d'> <c c'> <f f'> <d d'> |
  <g g'> <bes bes'> <g g'> <c g'> |
  <f f,> <ees ees,> <d d,> <c c,> |
  <bes bes,> <aes aes,> <g g,> <c c,> |
  <bes bes,> <aes aes,> <g g,> <f f,> |
  <ees ees,>1~ |
  
  q |
  q~ |
  q |
  q |
  q |
  <ees ees'>4 r <c c'> r |
  <f a c f>2 q \bar "|."
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
  >>
    \new PianoStaff << \new Staff { \new Voice { \global \pianoRH } } \new Staff { \clef "bass" \global \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
