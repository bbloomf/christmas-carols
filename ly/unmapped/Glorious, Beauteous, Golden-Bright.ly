\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Glorious, Beauteous, Golden-Bright"}}
    poet = \markup\oldStyleNum"Anna M. E. Nichols"
    composer = \markup\oldStyleNum"Maria Tiddeman (1837–1915)"
    tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #194
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

\markup \fill-line {\center-column{
    \concat{"Chorus and text of verses from " \italic"Piæ Cantiones" \oldStyleNum", 1582, via " \italic"imslp.org" ", Melody of verses from " \italic "www.cpdl.org"}}}
\markup\vspace#2.2























global = {
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \tieDotted
}

sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 2 fis4. g8 |
    a4 d cis b8[ a] |
    d2 d4 cis8[ d] |
    
    e4 d8[cis] b4 cis8[ b] |
    a2 a4 c |
    b a8[ g] fis4 e |
    d2 fis4. fis8 |
    
    g4 gis a a |
    a2 b4. b8 |
    cis4 d d8[ cis] b[ cis] |
    \partial 2 d2 |
  }
  \pageBreak
  \repeat volta 3 {
    \partial 2 a4. a8 |
    fis'4 d b e |
    \slurDotted
    d4( cis8[) b] a4. a8 |
    a4. a8 d4 d |
    
    d( cis) e4. d8 |
    cis4 b a a |
    a~ a fis4. fis8 |
    
    g4 gis a a |
    a2 b4. b8 |
    cis4 d d8[ cis] b[ cis] |
    \partial 2 d2 | \break
  }
  \partial 2 a4. a8 |
  fis'4 d b e |
  d4 cis8[ b] a4. a8 |
  a4 a d d |
  
  d cis e4. d8 |
  cis4 b a a |
  a^\markup{\italic"rall." \dynamic"ff"} a a4. a8 |
  
  d4 a fis g |
  b-> a a4.^\pp a8 |
  a4 a a^\markup\italic"rall." a 
  a2 a \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4. d8 |
  d4 fis e d8[ cis] |
  d2 fis4 fis |
  
  e fis fis e8[ d] |
  cis2 d4 e |
  d d d d8[ cis] |
  d2 d4. d8 |
  
  d4 d cis d8[ e] |
  d2 d4. d8 |
  e4 d g g |
  fis2 |
  
  
  fis4. fis8 |
  a4 a a g |
  \slurDotted
  g4.~ g8 g4. g8 |
  fis4. fis8 fis4 gis |
  
  a~ a g4. g8 |
  g4 g e fis |
  g( fis) d4. d8 |
  
  d4 d cis d8[ e] |
  d2 d4. d8 |
  e4 d g g |
  fis2 |
  
  
  fis4. fis8 |
  a4 a a g |
  g4. g8 g4. g8 |
  fis4 fis fis gis |
  
  a4 a g4. g8 |
  g4 g e fis |
  g fis a4. a8 |
  
  d,4 a' fis g |
  b a e4. fis8 |
  g4 fis e d |
  e2 fis \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Glo -- rious, beau -- teous, gol -- den -- bright,
  Shed -- ding soft -- est pur -- est light,
  Shone the stars that \set associatedVoice = "tenors" Christ -- mas night,
  When the Jew -- ish shep -- herds kept
  Watch be -- side \unset associatedVoice their flocks that slept.
  
  \set stanza = #"3. "
  Soft and pure and ho -- ly \set ignoreMelismata = ##t glo -- \unset ignoreMelismata ry,
  Kings and seers and pro -- phets \set ignoreMelismata = ##t hoa -- ry, \unset ignoreMelismata 
  Shed through -- out the sa -- cred \set ignoreMelismata = ##t sto -- ry: \unset ignoreMelismata 
  While the priests, like shep -- herds true,
  Watch’d be -- side God’s cho -- sen __ few.
  
  \set stanza = #"6. "
  Since that Light then dark -- ens nev -- er,
  Let us all, with glad en -- dea -- vor,
  Sing the song that e -- choes ev -- er:
  Glo -- ry in the high -- est Heav -- en!
  \set associatedVoice = "tenors"
  Peace on earth to us for -- giv -- en.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  But the stars’ sweet gold -- en gleam
  Fad -- ed quick -- ly as a dream
  ’Mid the won -- drous \set associatedVoice = "tenors" glo -- ry -- stream,
  That il -- lum -- ined all the earth,
  When Christ’s An -- \unset associatedVoice gels sang His birth.
  
  \set stanza = #"4. "
  But that light no more a -- \set ignoreMelismata = ##t vail -- \unset ignoreMelismata ed,
  All its splen -- dor straight -- way \set ignoreMelismata = ##t pal -- ed \unset ignoreMelismata 
  In His light whom An -- gels \set ignoreMelismata = ##t hail -- ed; \unset ignoreMelismata 
  E -- ven as the stars of old,
  ’Mid the bright  -- ness lost their gold.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \repeat unfold 35 { \skip 1 }
  \set stanza = #"5. "
  Now no more on Christ -- mas night, __
  Is the sky with An -- gels bright, __
  But for ev -- er shines the Light; __
  E -- ven He Whose birth they told
  To the shep -- herds by __ the __ fold.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  a4. a8 |
  a4 a a a |
  a2 b4 a |
  
  a4 a a gis |
  a2 fis4 g! |
  g4 g a8[ fis] a[ g] |
  fis2 fis4. fis8 |
  
  d4 e e fis8[ g] |
  fis2 g4. b8 |
  a4 a e' d8[ e] |
  d2 |
  
  
  d4. d8 |
  d4 d d e |
  \slurDotted
  e4.( d8) e4. e8 |
  d4. d8 d4 d |
  
  e~ e cis4. d8 |
  e4 d cis d |
  cis( d) a4. d,8 |
  
  d4 e e fis8[ g] |
  fis2 g4. b8 |
  a4 a e' d8[ e] |
  d2 |
  
  
  d4. d8 |
  d4 d d e |
  e4. d8 e4. e8 |
  d4 d d d |
  
  e e cis4. d8 |
  e4 d cis d |
  cis <<d {s8 s_\ff}>> a4. a8 |
  
  d4 a fis g |
  b a cis_\pp d |
  cis d cis b |
  cis2 d \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4. e8 |
  fis4 d g g |
  fis2 b,4 fis' |
  
  cis4 fis d e |
  a,2 d4 c |
  g' b, a a |
  d2 b4. b8 |
  
  b4 b a a |
  d2 g4. g8 |
  g4 fis a a, |
  d2 |
  
  
  d4. d8 |
  d4 fis g e |
  \slurDotted
  a4.( b8) cis4. cis8 |
  d4. d8 b4 b |
  
  a~ a a4. a8 |
  a4 a a d, |
  a'( d,) d4. b8 |
  
  b4 b a a |
  d2 g4. g8 |
  g4 fis a a, |
  d2 |
  
  
  d4. d8 |
  d4 fis g e |
  a4. b8 cis4. cis8 |
  d4 d b b |
  
  a a a4. a8 |
  a4 a a d, |
  a' d, a4. a8 |
  
  d4 a fis g |
  b a a a |
  a a a a |
  a2 d \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
     \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
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
    \tempo 4 = 100
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
