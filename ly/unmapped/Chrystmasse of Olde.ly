\version "2.14.2"
\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Chrystmasse of Olde"}}
  poet = \markup\oldStyleNum"Eugene Field (1850–1895)"
  composer = \markup\italic "Swiss Air"
  tagline = \markup \concat{ "from " \italic "Favorite Songs and Hymns for School and Home" \oldStyleNum", 1899, via " \italic"books.google.com"}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #144
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
global = {
  \key d \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 8*3 fis8 fis g |
    a4. fis'8 fis e |
    d4( fis,8) a b a |
    a cis, cis e b' a | \break
    
    a d, d fis fis g |
    a4. a8 fis' e |
    d4( fis,) a | \break
    a8[ gis b] a g e |
    
    d4 b'8\rest a b a |
    a cis, cis e b' a | \break
    a d, d a' b a |
    a cis, cis e b' a |
    
    a d, d fis fis g | \break
    a4. a8 fis' e |
    d4( fis,) a |
    a8[ gis b] a g e |
    \partial 8*3 d4. \break
  }
  \pageBreak
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 d e |
  fis4. a8 a g |
  fis4( d8) fis g fis |
  e a, a cis cis cis |
  
  d a a d d e |
  fis4. fis8 a g |
  fis4( d) fis |
  fis8[ e d] cis cis cis |
  
  d4 s8 fis g fis |
  e a, a cis e e |
  d d d fis g fis |
  e a, a cis e e |
  
  d d d d d e |
  fis4. fis8 a g |
  fis4( d) fis |
  fis8[ e d] cis cis cis |
  d4.
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  God rest you, Chryst -- en gen -- til men, 
    Wher -- ev -- er you may be,
    Wher -- ev -- er you may be,
  God rest you all in fielde or hall,
    Or on __ ye storm -- y sea; 
  For on this morn, this morn, oure Chryst is born, is born,
    That sav -- eth you and me,
    That sav -- eth you and me.
  For on this morn oure Chryst is born
    That sav -- eth you and me.
    
  \set stanza = #"3. "
  God rest you, Chryst -- en gen -- til men, 
    Far -- ing wher -- e’er you may,
    Far -- ing wher -- e’er you may;
  In no -- blesse court do thou no sport,
    In tour -- na -- ment no playe, 
  In Pay -- nim lands hold thou, hold thou thy hands, thy hands
    From bloud -- y works this daye,
    From bloud -- y works this daye.
  In Pay -- nim lands hold thou thy hands
    From bloud -- y works this daye.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Last night ye shep -- herds in ye east 
    Saw ma -- ny~a won -- drous thing,
    Saw ma -- ny~a won -- drous thing;
  Ye sky last night flamed pass -- ing bright
    Whiles that __ ye stars did sing, 
  And an -- gels came to bless, to bless ye name, ye name
    Of Je -- sus Chryst, oure Kyng,
    Of Je -- sus Chryst, oure Kyng.
  And an -- gels came to bless ye name
    Of Je -- sus Chryst, oure Kyng.
    
  \set stanza = #"4. "
  But think -- ing on ye gen -- til Lord 
    That died up -- on ye tree,
    That died up -- on ye tree,
  Let troub -- lings cease and deeds of peace
    A -- bound __ in Chryst -- an -- tie;
  For on this morn, this morn, oure Chryst is born, is born,
    That sav -- eth you and me,
    That sav -- eth you and me.
  For on this morn oure Chryst is born
    That sav -- eth you and me.
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
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
  a8 a a |
  d4. d8 d a |
  a4. d8 d d |
  cis e, e a g g |
  
  fis fis fis a a a |
  d4. d8 d a |
  a2 d4 |
  d8[ b gis] a a g |
  
  fis4 s8 d' d d |
  cis e, e a g g |
  fis fis fis d' d d |
  cis e, e a g g |
  
  fis fis fis a a a |
  d4. d8 d a |
  a2 d4 |
  d8[ b gis] a a g |
  fis4.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 d d |
  d4. d8 d d |
  d4. d8 d d |
  a a a a a a |
  
  d d d d d d |
  d4. d8 d d |
  d2 d4 |
  a4. a8 a a |
  
  d4 d8\rest d d d |
  a a a a a a |
  d d d d d d |
  a a a a a a |
  
  d d d d d d |
  d4. d8 d d |
  d2 d4 |
  a4. a8 a a |
  d4.
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
    >>
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
    \tempo 4 = 80
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
