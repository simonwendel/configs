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

import { describe, expect, inject, it } from '@angular/core/testing';
import { ComponentFixture, TestComponentBuilder } from '@angular/compiler/testing';

import { AppComponent } from './app.component';

describe('COMPONENT: app.component', () => {

    it('should be instantiable.', () => {
        inject([TestComponentBuilder], (componentBuilder: TestComponentBuilder) => {
            return componentBuilder
                .createAsync(AppComponent)
                .then((fixture: ComponentFixture<AppComponent>) => {
                    expect(fixture.componentInstance).toBeDefined();
                });
        });
    });

    it('should have a message field.', () => {
        let sut = new AppComponent();
        expect(sut.message).toEqual('Yo, this works!');
    });
});
