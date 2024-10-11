import { readBytesAsString } from "../../utils/byte_functions/string.js";
import { readLittleEndian } from "../../utils/byte_functions/little_endian.js";
import { DLSPreset } from "./dls_preset.js";
import { findRIFFListType, readRIFFChunk } from "../basic_soundfont/riff_chunk.js";
import { SpessaSynthGroup, SpessaSynthGroupEnd } from "../../utils/loggin.js";
import { BasicInstrumentZone } from "../basic_soundfont/basic_zones.js";
import { consoleColors } from "../../utils/other.js";

/**
 * @this {DLSSoundFont}
 * @param chunk {RiffChunk}
 */
export function readDLSInstrument(chunk)
{
    this.verifyHeader(chunk, "LIST");
    this.verifyText(readBytesAsString(chunk.chunkData, 4), "ins ");
    /**
     * @type {RiffChunk[]}
     */
    const chunks = [];
    while (chunk.chunkData.length > chunk.chunkData.currentIndex)
    {
        chunks.push(readRIFFChunk(chunk.chunkData));
    }
    
    
    const instrumentHeader = chunks.find(c => c.header === "insh");
    if (!instrumentHeader)
    {
        SpessaSynthGroupEnd();
        throw new Error("No instrument header!");
    }
    
    // read instrument header
    const regions = readLittleEndian(instrumentHeader.chunkData, 4);
    const ulBank = readLittleEndian(instrumentHeader.chunkData, 4);
    const ulInstrument = readLittleEndian(instrumentHeader.chunkData, 4);
    const preset = new DLSPreset(ulBank, ulInstrument);
    
    // read preset name in INFO
    let presetName = "unnamedPreset";
    const infoChunk = findRIFFListType(chunks, "INFO");
    if (infoChunk)
    {
        let info = readRIFFChunk(infoChunk.chunkData);
        while (info.header !== "INAM")
        {
            info = readRIFFChunk(infoChunk.chunkData);
        }
        presetName = readBytesAsString(info.chunkData, info.chunkData.length).trim();
    }
    preset.presetName = presetName;
    preset.DLSInstrument.instrumentName = presetName;
    SpessaSynthGroup(
        `%cParsing %c"${presetName}"%c...`,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info
    );
    
    // list of regions
    const regionListChunk = findRIFFListType(chunks, "lrgn");
    if (!regionListChunk)
    {
        SpessaSynthGroupEnd();
        throw new Error("No region list!");
    }
    
    // global articulation: essentially global zone
    const globalZone = new BasicInstrumentZone();
    globalZone.isGlobal = true;
    
    // read articulators
    const globalLart = findRIFFListType(chunks, "lart");
    const globalLar2 = findRIFFListType(chunks, "lar2");
    this.readLart(globalLart, globalLar2, globalZone);
    preset.DLSInstrument.instrumentZones.push(globalZone);
    
    // read regions
    for (let i = 0; i < regions; i++)
    {
        const chunk = readRIFFChunk(regionListChunk.chunkData);
        this.verifyHeader(chunk, "LIST");
        const type = readBytesAsString(chunk.chunkData, 4);
        if (type !== "rgn " && type !== "rgn2")
        {
            SpessaSynthGroupEnd();
            throw new SyntaxError(`Invalid DLS region! Expected "rgn " or "rgn2" got "${type}"`);
        }
        
        
        const zone = this.readRegion(chunk);
        if (zone)
        {
            preset.DLSInstrument.instrumentZones.push(zone);
        }
    }
    
    this.presets.push(preset);
    this.instruments.push(preset.DLSInstrument);
    SpessaSynthGroupEnd();
}