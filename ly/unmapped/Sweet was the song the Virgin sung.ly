\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Sweet was the song the Virgin sung"}}
    poet = \markup\concat{"From William Ballet’s " \italic"Lute Book" ", c. 1600"}
    composer = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 2)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.5)
       (stretchability . 0))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #170
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
%IF_NOT_LESSER
global = {
  \key f \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo \markup\medium\italic"At a moderate pace."
  \partial 2. g'4 g4. a8 |
  bes4 bes c bes |
  a2. b4\rest |
  b\rest d f d |
  
  ees d c4. bes8 |
  g2 bes4\rest d^\markup\italic"cres." |
  d d c4. d8 |
  c[ d] bes[ c] a2 |
  
  bes4\rest^\markup\italic"rall." f g a |
  bes g a4. a8 |
  g1 \bar "||"
  d'4^\markup\italic"a tempo" f c ees |
  
  bes8. c16^\< <<d8. {s8 s32 s32\!}>> c16 bes2^\> |
  a4\! c4 g bes |
  f8.^\markup\italic"cres." g16 a8 bes c4 ees |
  
  %page2
  d2.^\f^\> c8[\! bes] |
  a2. d4 |
  c4. bes8 a4 g |
  a4. g8 g2 |
  
  bes4\rest fis4^\markup\italic"cres." g a |
  bes4. a8 bes4 c |
  d2. b4\rest |
  b\rest f' ees d |
  
  c bes a g |
  fis4. fis8 g4 d |
  bes'4\rest a^\markup\italic"dim." bes g |
  d'4\rest d4 f d |
  
  c4. c8 c4 bes |
  a2 bes4\rest^\markup\italic"dim. e rall." f!4 |
  g a d2~ |
  d4 c8[ bes] a4. g8 |
  g1 \bar "|."
}
sopWords = \lyricmode {
  \repeat unfold 92 {\skip1}
  And rock’d Him sweet -- ly on her knee.
}

altoMusic = \relative c' {
  bes4 d4. f8 |
  f4 g g g |
  fis2. s4 |
  s a4 c bes |
  
  g g g f |
  ees2 s4 d4 |
  d d e f |
  g f8[ e] c2 |
  
  s4 d d e8[ fis] |
  g4 g g fis |
  g1 \bar "||"
  f4 f f g |
  
  d8. d16 d8. d16 d2 |
  f4 g ees f |
  f8. f16 ees8 d f4 g |
  
  %page2
  f2( g4) d |
  d2. d4 |
  ees4. d8 d4 bes |
  d d d2 |
  
  s4 d d f |
  f4. f8 d4 f |
  f2. s4 |
  s f g d |
  
  ees f ees d |
  d4. d8 d4 d |
  s fis d d |
  g2 f4 f |
  
  f4. f8 ees4 d |
  d2 s4 d |
  e f f2 |
  e a,4 d |
  d1 \bar "|."
}
dropLyrics =
{
    \override LyricText #'extra-offset = #'(0 . -2.0)
    \override LyricHyphen #'extra-offset = #'(0 . -2.0)
    \override LyricExtender #'extra-offset = #'(0 . -2.0)
}
altoWords = {
  \dropLyricsV
  \set stanza = \markup\dynamic"mp "
  \lyricmode {
    Sweet was the song the Vir -- gin 
    \set associatedVoice = "tenors"
    sung, When 
    \unset associatedVoice
    she, when she to Beth -- lem Ju -- da came,
    And was de -- liv -- er’d of a Son,
  }
  \set stanza = \markup\dynamic"  pp "
  \lyricmode{
    That bless -- ed Je -- sus hath \set associatedVoice = "sopranos" to "" name.
    \unset associatedVoice
  }
  \set stanza = \markup\dynamic"mp "
  \lyricmode{
    Lul -- la, lul -- la, lu -- la, lul -- la -- by,
    Lu -- la, lu -- la, lu -- la, lul -- la -- by, sweet
    
    %page2
    Babe, sung she,
  }
  \set stanza = \markup\dynamic" mf "
  \lyricmode{
    My Son, and eke a Sav -- ior born,
    Who hast vouch -- saf -- ed from on high
  }
  \set stanza = \markup\dynamic" f "
  \lyricmode{
    To vis -- it us
    \dropLyrics
    that were for -- lorn;
    \set associatedVoice = "tenors"
    La -- lu -- la, la -- lu --
    \unset associatedVoice
    la, la -- lu -- la --
    \dropLyricsV
    \set associatedVoice = "tenors"
    by,
  }
  \set stanza = \markup\dynamic"  p "
  \lyricmode{
    sweet babe, sang she,
    And rock’d Him sweet -- ly on her knee.
  }
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
  g4 bes4. c8 |
  d4 bes ees d |
  d2. d4 f2. f4 |
  
  ees4 bes c d |
  ees2 bes |
  a4 bes c a |
  g bes8[ g] a2 |
  
  s4 bes d c |
  bes bes d d |
  bes1 \bar "||"
  bes4 c c bes |
  
  bes8. bes16 a8. a16 g2 |
  d'4 c c f, |
  bes8. bes16 c8 bes a4 bes |
  
  %page2
  bes2. a8[ g] |
  fis2. g4 |
  g4. g8 fis4 g |
  g fis g2 |
  
  s4 a bes c |
  d4. c8 bes4 a |
  bes2. f'4 ees d bes4.( a8 |
  
  g4) f c' g |
  a4. a8 bes4 a |
  d2 bes4 bes |
  s bes c bes |
  
  a4. a8 g4 g |
  g( fis) s a |
  c c bes2 |
  g g4 fis |
  g1 \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 70 {\skip1}
  To vis -- it us __
}

bassMusic = \relative c {
  g'4 g4. f8 |
  bes,4 ees c g'|
  d2. d4\rest |
  d4\rest d a bes |
  
  c g' ees d |
  c2 g' |
  fis4 g c, f |
  e d8[ e] f2 |
  
  d4\rest bes' bes a |
  g ees d d |
  g1 \bar "||"
  
  bes4 a a g |
  
  g8. g16 fis8. fis16 g2 |
  f4 ees ees d |
  d8. d16 c8 g' f4 ees |
  
  %page2
  bes2( g4) bes |
  d2. bes4 |
  c4. g8 d'4 ees |
  d d g,2 |
  
  d'4\rest d g f |
  bes,4. f'8 g4 f |
  bes2. d,4\rest |
  d\rest bes'4 g4. f8 |
  
  ees4 d c4. bes8 |
  d4. d8 g4 fis |
  d4\rest d g g, |
  d'4\rest g a bes |
  
  f4. f8 c4 g' |
  d2 d4\rest d |
  c f bes,2 |
  c d4 d |
  g,1 \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 70 {\skip1}
  To vis -- it us
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
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
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
