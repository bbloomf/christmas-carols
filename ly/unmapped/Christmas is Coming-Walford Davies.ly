\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Christmas is Coming"}}
  %poet = \markup\oldStyleNum"Old Rhyme"
  composer = \markup\oldStyleNum"H. Walford Davies (1869–1941)"
  tagline = ""
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
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
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #010
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
  \time 2/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    ees8^\f g,16 g bes bes8 bes16 |
    c c c c bes8 bes\rest \bar "||"
    aes16 aes aes aes g g g g |
  }
  \alternative {
    {
      f8 f ees4
    }
    {
      f8 bes g8.[( bes16] |
    }
  }
  g8.[ bes16] g[ bes g bes] |
  g8) b\rest g bes16\rest g |
  
  g a bes c d d8 d16 |
  ees8 d16 c d8. d16 |
  ees8 d16 c d8. d16 |
  f8 d16 c bes4~^\> |
  
  bes2\!^\markup{\dynamic"pp" \italic"ad lib"} ~ |
  bes4. d8~^\markup\italic"cresc." |
  d d4 f8\fermata \bar "||"
  
  ees8^\markup{\dynamic"ff" \italic"a tempo"} g,16 g bes bes8 bes16 |
  c c c c bes8 bes\rest |
  aes16 aes aes aes g g g g |
  
  %page 2
  f8 f <<ees {s16 s^\markup{\dynamic"mf" \italic"(faster)"}}>> ees16 ees |
  ees f g aes bes bes8 bes16 |
  c8 bes16 aes bes8  ees,16 ees |
  
  ees f g aes bes8 bes16 bes |
  ees8 d16 c bes8 ees,16^\> ees |
  ees f g aes bes8^\p bes |
  bes2\rest |
  
  \time 6/8
  c2.^\p^\>^\markup\italic"Slowly" |
  ees\! \bar "||"
  bes4.~ <<bes4 {s8 s^\markup{\dynamic"f" \italic" Quickly"}}>> ees,8 |
  ees4 f8 g4 f8 |
  ees4 f8 g4 f8 |
  ees4 bes'8 bes4 bes8 |
  
  bes4.~ bes4 bes8 |
  c4 c8 bes4 g8 |
  bes4. aes4 g8 |
  f4 ees8 f4 g8 \bar "||"
  \time 2/2 aes2 g4^\f aes \break
  
  \repeat volta 2 {
    bes2 ees4 c |
    bes2 g4 aes |
    bes bes ees c |
    bes2 g4 aes |
    bes2 c4 g |
    aes4( f) ees d |
    
    ees4. f8 g4( ees) |
    aes2 g4 aes |
    bes2 c4 ees |
    ees c bes( g) |
  }
  \alternative {
    {
      ees1~ |
      ees2 g4 aes |
    }
    {
      ees1\fermata \bar "|."
    }
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 2 {
    g'8 g16 g g g8 g16 |
    aes aes aes aes g8 s |
    d16 ees f d ees ees ees ees |
  }
  \alternative {
    { ees8 d ees4 }
    { ees8 d ees4~ }
  }
  ees2~ |
  ees8 s ees s16 ees |
  
  ees ees ees ees d d8 g16 |
  g8 g16 g g8. g16 |
  g8 g16 g g8. g16 |
  f8 f16 f f4~ |
  
  f2~ |
  f4. f8~ |
  f f4 aes8 |
  
  g8 g16 g g g8 g16 |
  aes aes aes aes g8 s |
  d16 ees f d ees ees ees ees |
  
  %page 2
  ees8 d ees ees16 ees |
  ees d ees f g g8 g16 |
  aes8 g16 f g8 ees16 ees |
  
  ees d ees f g8 g16 g |
  aes8 g16 f g8 ees16 ees |
  ees f g aes bes8 bes |
  s2 |
  
  \time 6/8
  ees,2. |
  aes \bar "||"
  g4.~ g4 ees8 |
  ees4 f8 g4 f8 |
  ees4 f8 g4 f8 |
  ees4 bes'8 bes4 bes8 |
  
  bes4.~ bes4 g8 |
  aes4 aes8 g4 ees8 |
  g4. f4 ees8 |
  d4 c8 d4 ees8 \bar "||"
  \time 2/2 f2 ees4 ees |
  
  \repeat volta 2 {
    ees2 ees4 ees |
    ees2 ees4 ees |
    ees ees ees aes |
    g2 ees4 ees |
    ees2 ees4 ees |
    f( c) bes bes |
    
    ees4. d8 ees2 |
    f d4 f |
    ees2 ees4 aes |
    aes f d( bes) |
  }
  \alternative {
    {
    ees1~ |
    ees2 ees4 ees |
    }
    {
    ees1 \bar "|."
    }
  }
}
altoWords = \lyricmode {
  \dropLyricsVII
  Christ -- mas is com -- ing, the geese are get -- ting fat,
  Please to put a pen -- ny in the old man’s hat.
  old man’s hat. __
  
  If you have -- n’t got a pen -- ny, a ha’ -- p’ny -- ’ll do, a ha’ -- p’ny -- ’ll do, a ha’ -- p’ny -- ’ll do, __
  
  two, __ three, four!
  
  Christ -- mas is com -- ing, the geese are get -- ting fat,
  Please to put a pen -- ny in the old man’s hat.
  If you have -- n’t got a pen -- ny, a ha’ -- p’ny -- ’ll do,
  If you have -- n’t got a ha’ -- p’ny, a \pageBreak far -- thing -- ’ll do,
  If you have -- n’t got a far -- thing,
  
  God bless you! __
  
  God bless the mas -- ter of this house, like -- wise the mis -- tress too, __
  And all the lit -- tle chil -- dren that round the ta -- ble grow.
  
  Love and joy come to you, and to you your was -- sail too,
  And God bless you, and send you a hap -- py new year,
  And God send you a hap -- py new year. __
  
  Love and
  Year.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
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
  \repeat volta 2 {
    bes8 bes16 bes ees ees8 ees16 |
    ees ees ees ees ees8 f\rest |
    bes,16 bes bes bes bes bes bes bes |
  }
  \alternative {
    { bes8 bes g4 }
    { bes8 bes bes4~ }
  }
  bes2~ |
  bes8 s bes s16 bes |
  
  bes a g g d' d8 d16 |
  c8 bes16 a bes8. bes16 |
  c8 bes16 a bes8. bes16 |
  d8 bes16 a bes8[ d]~ |
  
  d2~ |
  d4. c8\rest |
  f4\rest f8\rest d8\fermata \bar "||"
  
  ees8 bes16 bes ees ees8 ees16 |
  ees16 ees ees ees ees8 ees\rest |
  bes16 bes bes bes bes bes bes bes |
  
  %page 2
  bes8 bes g bes16 bes |
  bes bes bes bes bes bes8 bes16 |
  ees8 ees16 ees bes8 bes16 bes |
  
  bes bes bes bes bes8 bes16 bes |
  c8 bes16 aes ees'8 ees,16 ees |
  ees f g aes bes8 bes |
  s2 |
  
  \time 6/8
  aes2. |
  c4.( aes) \bar "||"
  ees'4.~ ees4 s8 |
  s2.*6 |
  
  s4. bes4.~ |
  \time 2/2
  bes2~ bes4 c |
  
  \repeat volta 2 {
    g2 c4 aes |
    g2 bes4 bes |
    bes bes c ees |
    g2 bes,4 aes |
    g2 g4 c |
    c( aes) f bes |
    
    bes4. aes8 g4( bes) |
    c2 d4 c |
    bes2 ees4 c |
    c aes f( d') |
  }
  \alternative {
    {
      ees1~ |
      ees2 bes4 c |
    }
    { ees1 \bar "|." }
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 2 {
    ees8_\f ees16 ees ees ees8 ees16 |
    aes bes c aes d,8\rest ees^> |
    bes16 c d bes ees f g ees |
  }
  \alternative {
    { bes'8 bes, ees4 }
    { bes'8 bes, ees8.([ g16] }
  }
  ees8.[ g16] ees16[ g ees g] |
  ees8) d\rest ees d16\rest g |
  
  g f ees g bes bes8 bes,16 |
  c8 ees16 ees g8. g16 |
  c,8 ees16 ees g8. g16 |
  d8 f16 f bes4 |
  
  bes16 bes bes bes bes bes,8 bes'16 |
  \times 2/3 { bes16 bes bes } bes16 bes bes bes,8 bes16 |
  bes'8 d,16\rest bes16 bes'4\fermata \bar "||"
  
  ees,8_\ff ees16 ees ees ees8 ees16 |
  aes bes c aes d,8\rest ees8^> |
  f16 ees d bes ees f g ees |
  
  %page 2
  bes'8 bes, <<ees {s16 s^\markup\dynamic"mf"}>> ees16 ees |
  ees ees ees ees ees ees8 ees16 |
  ees8 ees16 ees ees8 ees16 ees |
  
  ees ees ees ees ees8 ees16 ees |
  ees8 ees16 ees ees8 ees16_\> ees |
  ees f g aes bes8_\p bes |
  d,2\rest |
  
  \time 6/8
  aes'2._\p_\> |
  aes,4.(\! c) \bar "||"
  ees4.~ ees4 d8\rest |
  d2.\rest |
  d\rest |
  d\rest |
  
  d\rest |
  d\rest |
  d\rest |
  d4\rest d8\rest^\mf bes'4.~^\< |
  <<bes2(\! {s4. s8^\f}>> ees,4) ees |
  
  \repeat volta 2 {
    ees2 ees4 ees |
    ees2 ees4 f |
    g g aes c |
    ees2 ees,4 f |
    g2 c,4 c |
    f2 bes,4 bes |
    
    g'4. f8 ees4( g) |
    f2 bes4 aes |
    g2 aes4 c |
    f,8[ g] aes4 bes,( bes') |
  }
  \alternative {
    {
      ees,1~ |
      ees2 ees4 ees |
    }
    { ees1\fermata \bar "|." }
  }
}
bassWords = \lyricmode {
  \repeat unfold 10 { \skip 1 }
  fat,
  \repeat unfold 37 {\skip 1}
  But a pen -- ny’s bet -- ter,
  A pen -- ny or two are bet -- ter, or three! or four!
  
  \repeat unfold 10 { \skip 1}
  fat,
  \repeat unfold 48 {\skip 1}
  Love __
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.8 }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 8)
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
  
}

\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \midi {
    \tempo 4 = 70
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

