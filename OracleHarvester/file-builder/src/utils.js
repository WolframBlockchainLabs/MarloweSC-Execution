import { CONTAINER_NAME } from './config';

export const buildCommand = (command) => {
    return `docker exec ${CONTAINER_NAME} sh -c "${command}"`;
}

export const calculateTimeInMilliseconds = (offsetSeconds = 0) => {
	const nowInSeconds = Math.floor(Date.now() / 1000);
	const pastInSeconds = nowInSeconds + offsetSeconds;
	const resultInMilliseconds = pastInSeconds * 1000;
	return resultInMilliseconds;
}

import fs from 'fs';

export const readMyFile = (filePath) => {
	try {
		const data = fs.readFileSync(filePath, 'utf8');
		const jsonData = JSON.parse(data);
		return jsonData;
	} catch (error) {
		console.error('Error reading or parsing file:', error);
	}
}

export const writeMyFile = (data, filePath) => {
	try {
		const jsonData = JSON.stringify(data, null, 4); 
		fs.writeFileSync(filePath, jsonData, 'utf8');
		console.log('JSON file successfully written.');
	} catch (err) {
		console.error('Error writing JSON:', err);
	}
}

export const createMyDir = (absPath) => {
	try {
		if (!fs.existsSync(absPath)) {
			fs.mkdirSync(absPath);
			console.log('Successfully created directory.');
		} else {
			console.log('Directory already exists.');
		}
		return absPath;
	} catch (err) {
		console.error('Error creating directory:', err);
	}
}

import { exec as execCallback } from 'child_process';
import { promisify } from 'util';

const exec = promisify(execCallback);

export const my_exec = async (command) => {
    try {
        const { stdout, stderr } = await exec(command);
        if (stderr) {
            console.error(`stderr: ${stderr}`);
        }
        console.log(`stdout: ${stdout}`);
    } catch (error) {
        console.error(`Error: ${error.message}`);
    }
}