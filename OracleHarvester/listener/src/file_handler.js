import fs from 'fs';

import { NETWORK } from './config';

import path from 'path';

export async function readJSON(logger) {
    try {
        const content = await fs.promises.readFile(`../check_point/${NETWORK}.json`, 'utf-8');
        return JSON.parse(content);
    } catch (error) {
        logger.error(error);
        return null;
    }
}

export async function writeJSON(data, logger) {
    const directoryPath = path.join(__dirname, '../check_point');
    const filePath = path.join(directoryPath, `${NETWORK}.json`);
    try {
        await fs.promises.mkdir(directoryPath, { recursive: true });
        const dataStringify = JSON.stringify(data, null, 4);
        await fs.promises.writeFile(filePath, dataStringify);
        logger.info('Writing data to file.');
    } catch (error) {
        logger.error(error);
    }
}