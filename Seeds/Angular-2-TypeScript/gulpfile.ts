/**
 * @license
 * {one line to give the program's name and a brief idea of what it does.}
 * Copyright (C) {year}  Simon Wendel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import { Gulpclass, SequenceTask, Task } from 'gulpclass/Decorators';

/* Configuration. */
const DIRECTORIES = {
    BASE: './',
    APP: './app/'
};

const FILES = {
    TYPESCRIPTS: DIRECTORIES.APP + '**/*.ts',
    JAVASCRIPTS: DIRECTORIES.APP + '**/*.js',
    SOURCEMAPS: DIRECTORIES.APP + '**/*.js.map',
    TSCONFIG: 'tsconfig.json',
    KARMACONF: 'karma.conf.js'
};

/* Hack: Global require should always be available at runtime. */
declare function require(name: string): any;

/* Gulp modules and stuff. */
const gulp = require('gulp'),
    del = require('del'),
    tsc = require('gulp-typescript'),
    sourcemaps = require('gulp-sourcemaps'),
    Server = require('karma').Server,
    tslint = require('gulp-tslint'),
    remapIstanbul = require('remap-istanbul/lib/gulpRemapIstanbul'),
    webserver = require('gulp-webserver');

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(FILES.TSCONFIG);

    /**
     * DEFAULT.
     * Runs TDD.
     */
    @SequenceTask('default')
    defaultTask() {
        return ['clean', 'watch', 'tdd'];
    }

    /**
     * CLEAN.
     * Cleans compiled files from the application directory.
     */
    @Task()
    clean() {
        return del([
            FILES.JAVASCRIPTS,
            FILES.SOURCEMAPS]);
    }

    /**
     * WATCH.
     * Watches TypeScript files in the application directory and compiles them
     * whenever they change.
     */
    @Task()
    watch(done: Function) {
        gulp.watch(FILES.TYPESCRIPTS, ['compile']);
        done();
    }

    /**
     * LINT.
     * Runs tslint on all TypeScript source files in the application directory.
     */
    @Task()
    lint() {
        return gulp.src(FILES.TYPESCRIPTS)
            .pipe(tslint())
            .pipe(tslint.report('prose', {
                summarizeFailureOutput: true,
                emitError: false
            }));
    }

    /**
     * COMPILE.
     * Builds TypeScript source files into the application directory.
     */
    @Task('compile', ['lint'])
    compile() {
        const tsResult = this.tsProject.src()
            .pipe(sourcemaps.init())
            .pipe(tsc(this.tsProject));

        return tsResult.js
            .pipe(sourcemaps.write(DIRECTORIES.BASE, {sourceRoot: DIRECTORIES.APP}))
            .pipe(gulp.dest((file) => file.base));
    }

    /**
     * START.
     * Compiles, watches and launches web server with live-reload.
     */
    @Task('start', ['compile', 'watch'])
    start() {
        return gulp.src(DIRECTORIES.BASE)
            .pipe(webserver({
                livereload: true,
                open: true
            }));
    }

    /**
     * TDD.
     * Runs the jasmine test suite in the karma runner, with file watches
     * and repeated runs on file changes.
     */
    @Task('tdd', ['compile', 'watch'])
    tdd(done: Function) {
        new Server({
            configFile: __dirname + '/' + FILES.KARMACONF,
            autoWatch: true,
            singleRun: false
        }, done).start();
    }

    /**
     * TEST.
     * Runs the jasmine test suite in the karma runner, but only once.
     */
    @Task('test', ['compile'])
    test(done: Function) {
        new Server({
            configFile: __dirname + '/' + FILES.KARMACONF,
            autoWatch: false,
            singleRun: true
        }, done).start();
    }

    /**
     * DELETE-COVERAGE.
     * Deletes old code coverage data, if present.
     */
    @Task('delete-coverage')
    deleteCoverage() {
        return del('./coverage');
    }

    /**
     * COVERAGE.
     * Runs the jasmine test suite in the karma runner, once, and then
     * remaps the Istanbul code coverage data back to the original
     * TypeScript files.
     */
    @Task('coverage', ['delete-coverage', 'test'])
    coverage() {
        return gulp.src('./coverage/coverage-final.json')
            .pipe(remapIstanbul({
                useAbsolutePaths: true,
                reports: {
                    'html': './coverage/html',
                    'json': './coverage/coverage-remapped.json'
                }
            }));
    }
}
