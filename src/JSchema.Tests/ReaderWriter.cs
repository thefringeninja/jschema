﻿// Copyright (c) Mount Baker Software.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;

namespace MountBaker.JSchema.Tests
{
    internal static class ReaderWriter
    {
        public static IEnumerable<object[]> TestCases = new[]
        {
            new object[]
            {
                "Empty",
                new JsonSchema()
            },

            new object[]
            {
                "Basic",
                new JsonSchema
                {
                    Id = new Uri("http://www.example.com/schemas/basic#"),
                    SchemaVersion = JsonSchema.V4Draft,
                    Title = "The title",
                    Description = "The description",
                    Type = JsonType.Object
                }
            },

            new object[]
            {
                "Properties",
                new JsonSchema
                {
                    Type = JsonType.Object,

                    Properties = new Dictionary<string, JsonSchema>
                    {
                        ["stringProp"] = new JsonSchema
                        {
                            Type = JsonType.String
                        },

                        ["numberProp"] = new JsonSchema
                        {
                            Type = JsonType.Number
                        },

                        ["booleanProp"] = new JsonSchema
                        {
                            Type = JsonType.Boolean
                        },

                        ["integerProp"] = new JsonSchema
                        {
                            Type = JsonType.Integer
                        },
                    }
                }
            }
        };
    }
}