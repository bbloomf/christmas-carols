\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Joy to the World!"}}
  poet = \markup\oldStyleNum"Isaac Watts (1674–1748)"
  composer = \markup\oldStyleNum"Lowell Mason (1792–1872)"
  tagline = \markup { "from" \concat{\italic "Hymns of the Kingdom of God" \oldStyleNum", 1910, via " \italic"HymnsAndCarolsOfChristmas.com"}}
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
  first-page-number = #014
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
  \key d \major
  \time 2/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0.5 . 1)
}

sopMusic = \relative c'' {
  \tempo 4 = 70
  d4 cis8.\noBeam b16 |
  a4. g8 |
  fis4 e |
  d4. a'8 |
  b4. b8 | \break
  
  cis4. cis8 |
  d4.\fermata d8 |
  d[ cis] b[ a] |
  a8.[ g16 fis8] d' |
  d[ cis] b[ a] | \break
  
  a8.[ g16 fis8] fis |
  fis\noBeam fis\noBeam fis\noBeam fis16[ g] |
  a4. g16[ fis] |
  e8\noBeam e\noBeam e\noBeam e16[ fis] | \break
  
  g4. fis16[ e] |
  d8( d'4) b8 |
  a8.[ g16 fis8] g |
  fis4 e |
  d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  fis4 a8.\noBeam g16 |
  fis4. e8 |
  d4 cis |
  d4. a'8 |
  g4. g8 |
  
  g4. g8 |
  fis4. fis8 |
  fis8[ a] g[ fis] |
  fis8.[ e16 d8] fis |
  fis[ a] g[ fis] |
  
  fis8.[ e16 d8] d |
  d\noBeam d\noBeam d\noBeam d16[ e] |
  fis4. e16[ d] |
  cis8\noBeam cis\noBeam cis\noBeam cis16[ d] |
  
  e4. d16[ cis] |
  d8( fis4) g8 |
  fis8.[ e16 d8] e8 |
  d4 cis |
  d2
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Joy to the world! the Lord is come; Let earth re -- ceive her King; Let ev -- ’ry __ heart __ pre -- pare __ Him
  \raiseLyrics
  room, __
  And heav’n and na -- ture sing,
  And heav’n and na -- ture sing,
  \dropLyricsXV
  And heav’n, and heav’n __ and na -- ture sing.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Joy to the world! the Sav -- ior reigns; Let men their songs em -- ploy;
  While fields and __ floods, __ rocks, hills __ and __
  \raiseLyrics
  plains __
  
  Re -- peat the sound -- ing joy,
  Re -- peat the sound -- ing joy,
  \dropLyricsXV
  Re -- peat, __ re -- peat __ the sound -- ing joy.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  He rules the world with truth and grace And makes the na -- tions prove The glo -- ries __ of __ His right -- eous --
  \raiseLyrics
  ness, __
  
  And won -- ders of His love,  
  And won -- ders of His love,
  \dropLyricsXV
  And won -- ders, won -- ders of His love.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c' {
  a4 d8.\noBeam d16 |
  d4. b8 |
  a4. g8 |
  fis4. d'8 |
  d4. d8 |
  
  a4. a8 |
  a4. a8 |
  a4 d |
  d( a8)\noBeam a8 |
  a4 d |
  
  d( a8) d,\rest |
  d4\rest d8\rest a' |
  a\noBeam a\noBeam a\noBeam a\noBeam |
  a2~ |
  
  a4. a16[ g] |
  fis8( a4) d8 |
  d4( a8)\noBeam b |
  a4. g8 |
  fis2
}
tenorWords = \lyricmode {
  \repeat unfold 22 { \skip 1 }
  And heav’n and na -- ture sing, __
}
tenorWordsII = \lyricmode {
  \repeat unfold 22 { \skip 1 }
  Re -- peat the sound -- ing joy, __
}
tenorWordsIII = \lyricmode {
  \repeat unfold 22 { \skip 1 }
  And won -- ders of His love, __
}

bassMusic = \relative c {
  d4 d8.\noBeam d16 |
  d4. g,8 |
  a4 a |
  d4. fis8 |
  g4. g8 |
  
  a4. a8 |
  d,4.\fermata d8 |
  d4 d |
  d4. d8 |
  d4 d |
  
  d4. s8 |
  s4. d8 |
  d\noBeam d\noBeam d\noBeam d\noBeam |
  a'4. a,8 |
  
  a8\noBeam a\noBeam a\noBeam a\noBeam |
  d4. d8 |
  d4. g,8 |
  a4 a |
  d2
}
bassWords = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  And heav’n and na -- ture sing,
  And heav’n and na -- ture sing.
}
bassWordsII = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  Re -- peat the sound -- ing joy,
  Re -- peat the sound -- ing joy.
}
bassWordsIII = \lyricmode {
  \repeat unfold 28 { \skip 1 }
  And won -- ders of His love,
  And won -- ders of His love.
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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWordsII
%    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWordsIII
%    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWordsIII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
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
    \tempo 4 = 70
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
