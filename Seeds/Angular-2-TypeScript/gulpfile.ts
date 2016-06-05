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

import {Gulpclass, Task, SequenceTask} from 'gulpclass/Decorators';

/* Configuration. */
const BASEDIR = './';
const APPDIR = './app/';

const FILES = {
    TYPESCRIPTS: APPDIR + '**/*.ts',
    JAVASCRIPTS: APPDIR + '**/*.js',
    SOURCEMAPS: APPDIR + '**/*.js.map',
    TSCONFIG: 'tsconfig.json',
    KARMACONF: 'karma.conf.js'
};

/* Global require should always be available at runtime. */
declare function require(name: string): any;

/* Gulp modules and stuff. */
const gulp = require('gulp');
const del = require('del');
const tsc = require('gulp-typescript');
const sourcemaps = require('gulp-sourcemaps');
const Server = require('karma').Server;
const tslint = require('gulp-tslint');
const remapIstanbul = require('remap-istanbul/lib/gulpRemapIstanbul');

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(FILES.TSCONFIG);

    /**
     * DEFAULT.
     * Runs TDD.
     */
    @SequenceTask()
    default() {
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
            .pipe(sourcemaps.write(BASEDIR, {sourceRoot: APPDIR}))
            .pipe(gulp.dest((file) => file.base));
    }

    /**
     * TDD.
     * Runs the jasmine test suite in the karma runner, with file watches
     * and repeated runs on file changes.
     */
    @Task('tdd', ['compile'])
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
                // basePath: __dirname,
                useAbsolutePaths: true,
                reports: {
                    'html': './coverage/html',
                    'json': './coverage/coverage-remapped.json'
                }
            }));
    }

}
