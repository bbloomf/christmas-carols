\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jingle Bells"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(The One Horse Open Sleigh)"}}
  poet = \markup\oldStyleNum"James Lord Pierpont (1822–1893)"
  composer = \markup\oldStyleNum"James Lord Pierpont (1822–1893)"
  tagline = \markup \concat{ \italic"The One Horse Open Sleigh" \oldStyleNum", 1857"}
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
  last-bottom-spacing =
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
  first-page-number = #114
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
%6.14 #(set-global-staff-size 14.1) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.1 20))) }
global = {
  \key aes \major
  \time 2/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \autoBeamOff
}

sopMusic = \relative c' {
  \tempo "Allegro"
  \slurDashed
  \oneVoice
  s2*7 | \break
  
  r4 r8 \teeny ees8 | \normalsize
  ees c' bes aes |
  \tieDashed ees4 r8 \teeny ees16~ ees16 | \normalsize
  \tieSolid
  ees8 c' bes aes|
  f4. \teeny f8 | \normalsize
  f des' c bes |
  
  g4. \teeny g8 | \normalsize
  g f' ees des |
  c4. \teeny ees,8 | \normalsize
  ees c' bes aes |
  ees4. \teeny ees8 | \normalsize
  ees c' bes aes |
  
  f4. \teeny f8 | \normalsize
  f des' c bes |
  g ees' d ees |
  f ees des bes |
  aes2 \bar "||" \break
  
  %page2
  \voiceOne
  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
  \mark\markup\smallCapsOldStyle"chorus"
  c8 c c4 |
  ees8 ees ees4 |
  c8 c f8. f16 |
  e2 |
  f8 des aes f' |
  
  ees8 c aes aes16 bes |
  c8 bes aes bes |
  c2 |
  c8 c c4 |
  ees8 ees ees4 |
  
  %page3
  c8 c f f |
  e2 |
  f,8 des' c bes |
  aes ees' d ees16 ees |
  f8 ees des bes aes4 b\rest \bar"||"
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  s2*24 |
  
  %page2
  ees8 ees ees4 |
  ees8 ees ees4 |
  aes8 aes aes aes |
  g2 |
  f8 f f f |
  
  aes8 aes aes ees16 ees |
  ees8 ees ees ees |
  ees2 |
  aes8 aes aes4 |
  bes8 bes bes4 |
  
  %page3
  aes8 aes aes aes |
  g2 |
  f8 f f f |
  ees aes aes aes16 aes |
  g8 g g g |
  aes4 s \bar"||"
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  \skip1 Dash -- ing thro’ the snow
    In a one -- horse o -- pen sleigh, \skip1
  O’er the fields we go,
    \skip1 Laugh -- ing all the way;
  \skip1 Bells on bob  tail ring, \skip1
    Mak -- ing spi -- rits bright;
  \skip1 O what sport to ride and sing
    A sleigh -- ing song to -- night.
}
altoChorusWords = \lyricmode {
  \dropLyricsXII
  
  Jin -- gle bells,
    Jin -- gle bells,
    Jin -- gle all the way;
  Oh! what joy it is to ride
    In a one -- horse o -- pen sleigh.
    
  Jin -- gle bells,
    Jin -- gle bells,
    Jin -- gle all the way;
  Oh! what joy it is to ride
    In a one -- horse o -- pen sleigh.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  A day or two a -- go I __ _ thought I’d take a ride,
  And soon Miss Fan -- nie Bright Was seat -- ed by my side;
  The horse was lean and lank,
  Mis -- for -- tune seem’d his lot.
  He got in -- to a drift -- ed bank, And we, we got up -- sot.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  \skip1 Now the ground is white, \skip1\skip1
  Go it while you’re young, \skip1
  Take the girls to -- night, And sing this sleigh -- ing song;
  Just get a bob -- tailed bay,
  Two -- for -- ty as his speed,
  \skip1 Hitch him to an o -- pen sleigh
  And crack, you’ll take the lead.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \autoBeamOff
  \slurDashed
  s2*24 |
  
  %page2
  aes8 aes aes4 |
  bes8 bes bes4 |
  c8 c c c |
  c2 |
  aes8 aes aes aes |
  
  c8 c c c16 c |
  des8 des des des |
  c2 |
  ees8 ees ees4 |
  ees8 ees ees4 |
  
  %page3
  c8 c c c |
  c2 |
  aes8 aes des des |
  c c c c16 c |
  des8 des des des |
  c4 s \bar "||"
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
\autoBeamOff
\slurDashed
  s2*24 |
  
  %page2
  aes8 aes aes4 |
  g8 g g4 |
  f8 f f f |
  c2 |
  des8 des des des |
  
  ees8 ees ees ees16 ees |
  ees8 ees ees ees |
  aes,2 |
  aes'8 aes aes4 |
  g8 g g4 |
  
  %page3
  f8 f f f |
  c2 |
  des8 des des des |
  ees ees ees ees16 ees |
  ees8 ees ees ees |
  aes,4 d4\rest \bar"||"
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  \set Staff.ottavationMarkups = #ottavation-ordinals
  <c ees>8[ c' bes aes] |
  <ees c~>2 |
  q8[ c' bes aes] |
  <f des~>2 |
  q8[ des' c bes] |
  g[ ees' d ees] |
  g16[ f ees des] c[ bes aes g] |
  
  aes4 r |
  <ees c>8[ c' bes aes] |
  <ees c~>2 |
  q8[ c' bes aes] |
  <f des~>2 |
  q8[ des' c bes] |
  
  <g ees>2 |
  r8 <bes g ees>8[ q q] |
  r8 <aes ees c>[ q q] |
  <ees c>8[ c' bes aes] |
  <ees c~>2 |
  q8[ c' bes aes] |
  
  <f des~>2 |
  q8[ des' c bes] |
  r8 <g ees bes> r q |
  r <g ees des bes> r q |
  <aes ees c>2 |
  
  %page2
  <aes' c>16[ ees' q ees] q[ ees q ees] |
  <bes ees,>[ ees q ees] q[ ees q ees] |
  <aes, f>[ c q c] q[ c q c] |
  <g e>[ c g e] c[ e g c] |
  r f[ des f] aes,[ f' f, f'] |
  
  r ees[ c ees] aes,[ ees' ees, ees'] |
  r ees[ ees, ees'] f,[ ees' g, ees'] |
  aes,[ ees' c ees] aes4 |
  \ottava #1
  r16 ees[ <c' ees> ees,] q[ ees q ees] |
  r ees[ <bes' ees> ees,] q[ ees q ees] |
  
  %page3
  r c[ <aes' c> c,] q[ c q c] |
  e[ c' g e] \ottava #0 c[ g e g] |
  r f[ f' f,] ees'[ f, des' f,] |
  c'[ aes ees' aes,] d[ aes ees' aes,] |
  f'[ g, ees' g,] des'[ g, bes g] |
  aes8 r <aes c ees aes>4 \bar"||"
  
  \ottava #1
  ees'16[ c c' c,] bes'[ c, aes' c,] |
  ees[ c ees c] ees[ c ees c] |
  ees[ c c' c,] bes'[ c, aes' c,] |
  f[ des f des] f[ des f des] |
  f[ des des' des,] c'[ des, bes' des,] |
  aes'[ c, ees c] d[ c ees c] |
  g'[ f ees des] c[ bes aes g] |
  aes8 \ottava #0 r <aes, ees c>4 \bar "|."
}
pianoLH = \relative c {
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn
  << {aes8[ aes' ees aes]
      aes,8[ aes' ees aes]
      aes,8[ aes' ees aes]
      des,[ aes' f aes]
      des,[ bes' f bes]
      ees,2
      ees } \\
     {aes,2 | aes | aes | des | des | ees8[ bes' g bes] |
      ees,[ ees' g, des'] } >> |
     
  <aes c>4 r |
  << {aes,8[ aes' ees aes] |
      aes,8[ aes' ees aes] |
      aes,8[ aes' ees aes] |
      des,[ aes' f aes] |
      des,[ bes' f bes] |
     
      ees,[ bes' g bes] | } \\
     {aes,2 | aes | aes | des | des
     
      ees | } >>
  ees4 r |
  aes, r |
  << {aes8[ aes' ees aes]
      aes,8[ aes' ees aes]
      aes,8[ aes' ees aes]
      
      des,[ aes' f aes]
      des,[ bes' f bes]} \\
     {aes,2 | aes | aes |
     
      des | des } >> |
  ees8 r ees8 r |
  ees r ees r |
  <aes aes,>2 |
  
  %page2
  q8[ <aes c ees> q] r |
  <g g,>[ <g bes ees> q] r |
  <f f,>[ <f aes c> q] r |
  <c c,>[ <c e g c> q] r |
  <des des,>[ <f aes des> q] r |
  
  <ees ees,>[ <ees aes c> q] r |
  <ees ees,>[ <ees bes' des> q q] |
  aes,[ <ees' aes c>] q4 |
  aes,8[ <aes' c ees> q] r |
  g,[ <g' bes ees> q] r |
  
  %page3
  f,8 <f' aes c>[ q] r |
  c,8 <c' e g c>[ q] r |
  <des des,>8[ <f aes des> q] r |
  <ees ees,>[ <ees aes c> q] r |
  <ees ees,>[ <ees g bes des> q] r |
  <aes aes,>[ <ees aes c>] <aes, aes,>4 \bar"||"
  
  aes'8[ <c ees> q q] |
  aes[ q q q] |
  aes[ q q q] |
  des,[ <f aes des> q q] |
  des[ q q q ] |
  \once \override NoteHead.duration-log = #1 ees[ <aes c> q q] |
  \once \override NoteHead.duration-log = #1 ees[ <bes' des> q q] |
  <aes c> r <aes, aes,>4 \bar"|."
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . 0.5))} \lyricsto "sopranos" \altoWords
    \new Lyrics = "altosChorus"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -1))} \lyricsto "altos" \altoChorusWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
    \new PianoStaff << \new Staff { \global \new Voice { \pianoRH } } \new Staff { \global \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves
      \override VerticalAxisGroup.remove-first = ##t
      \override VerticalAxisGroup.minimum-Y-extent = #'(-3 . 3)
      \override VerticalAxisGroup.staff-staff-spacing =
      #'((basic-distance . 5)
         (minimum-distance . 1)
         (padding . 1)
         (stretchability . 2))
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

